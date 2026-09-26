(ns csl.daemon
  "The daemon (`csl index`): a disposable queue worker (DESIGN.md §6).

  At most one runs per machine: it holds daemon.lock for its whole life.
  It works the queue, collects garbage once it has been idle a while, and
  exits after a longer idle time or when a client sets stop_requested (a
  client of a newer version replacing it). All coordination goes through
  the database and the lock; there is no network API. New work is noticed
  by polling SQLite's data_version, which changes when another connection
  commits."
  (:refer-clojure :exclude [run!])
  (:require
   [clojure.java.io :as io]
   [csl.db :as db]
   [csl.gc :as gc]
   [csl.indexer :as indexer]
   [csl.lock :as lock]
   [csl.version :as version]))

(set! *warn-on-reflection* true)

(defn paths
  "The files under a csl home dir (default ~/.cache/clojure-sqlite-lsp)."
  [home]
  {:home (str home)
   :db (str (io/file home "index.db"))
   :daemon-lock (str (io/file home "daemon.lock"))
   :spawn-lock (str (io/file home "spawn.lock"))
   :log (str (io/file home "daemon.log"))})

(def defaults
  {:poll-ms 150
   :gc-after-idle-ms (* 30 1000)
   :idle-exit-ms (* 10 60 1000)
   :heartbeat-ms 5000
   ;; after the machine failed (a full disk), before trying the queue again
   :retry-ms 5000})

(defn- pid [] (.pid (java.lang.ProcessHandle/current)))

(defn- register! [c version]
  (let [now (System/currentTimeMillis)]
    (db/execute! c "INSERT INTO daemon (id, pid, version, started_at, heartbeat_at, stop_requested)
                    VALUES (1, ?, ?, ?, ?, 0)
                    ON CONFLICT (id) DO UPDATE SET pid = excluded.pid, version = excluded.version,
                      started_at = excluded.started_at, heartbeat_at = excluded.heartbeat_at,
                      stop_requested = 0"
                 (pid) version now now)))

(defn- unregister! [c]
  (db/execute! c "DELETE FROM daemon WHERE id = 1 AND pid = ?" (pid)))

(defn- stop-requested? [c]
  (= 1 (db/query-value c "SELECT stop_requested FROM daemon WHERE id = 1")))

(defn- data-version [c] (db/query-value c "PRAGMA data_version"))

(defn- work-loop
  "Run until asked to stop or idle long enough. Returns :stopped or :idle."
  [{:keys [c w] :as ix} {:keys [poll-ms gc-after-idle-ms idle-exit-ms heartbeat-ms retry-ms]}]
  (loop [last-work (System/currentTimeMillis)
         collected? true
         seen-version nil
         last-beat 0]
    (let [now (System/currentTimeMillis)
          dv (data-version c)]
      (when (> (- now last-beat) heartbeat-ms)
        (db/execute! c "UPDATE daemon SET heartbeat_at = ? WHERE id = 1" now))
      (if (stop-requested? c)
        :stopped
        (let [beat (if (> (- now last-beat) heartbeat-ms) now last-beat)
              ;; something changed (or we just started): work the queue
              worked (when (not= dv seen-version) (indexer/step! ix))]
          (cond
            (= :retry worked)
            (do (Thread/sleep (long retry-ms))
                (recur (System/currentTimeMillis) collected? nil beat))

            worked
            (recur (System/currentTimeMillis) false nil beat)

            (and (not collected?) (> (- now last-work) gc-after-idle-ms))
            (do (try (gc/collect! w {})
                     (catch Exception e
                       (binding [*out* *err*] (println "csl: garbage collection failed:" (ex-message e)))))
                (recur last-work true (data-version c) now))

            (> (- now last-work) idle-exit-ms) :idle

            :else
            (do (Thread/sleep (long poll-ms))
                (recur last-work collected? dv beat))))))))

(def ^:private lock-patience-ms
  "How long a starting daemon keeps trying daemon.lock. Clients probe
  liveness by taking it for an instant, so failing once doesn't mean
  another daemon runs; a running daemon holds it for good."
  2000)

(defn- take-daemon-lock [path]
  (let [deadline (+ (System/currentTimeMillis) lock-patience-ms)]
    (loop []
      (or (lock/try-lock path)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 10)
            (recur))))))

(defn run!
  "Run the daemon in this process until it stops. Returns :stopped, :idle,
  or :already-running when another daemon holds the lock."
  [{:keys [home version] :or {version version/version} :as opts}]
  (let [{:keys [db daemon-lock home]} (paths home)
        opts (merge defaults opts)]
    (if-let [held (take-daemon-lock daemon-lock)]
      (try
        (with-open [^java.io.Closeable ix (indexer/indexer {:db-path db :cache-dir (io/file home)})]
          (register! (:c ix) version)
          (try
            (work-loop ix opts)
            (finally
              (unregister! (:c ix)))))
        (finally
          (lock/release! held)))
      :already-running)))
