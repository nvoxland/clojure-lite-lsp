(ns clojure-lite-lsp.daemon
  "The daemon (`clojure-lite-lsp index`): a disposable queue worker.

  At most one runs per machine: it holds daemon.lock for its whole life.
  It works the queue, collects garbage once it has been idle a while, and
  exits after a longer idle time or when a client sets stop_requested (a
  client of a newer version replacing it). All coordination goes through
  the database and the lock; there is no network API. New work is noticed
  by polling SQLite's data_version, which changes when another connection
  commits."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.gc :as gc]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.log :as log]
   [clojure-lite-lsp.schema :as schema]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io])
  (:import
   [java.io Closeable File]
   [java.lang ProcessHandle]))

(set! *warn-on-reflection* true)

(def ^:private unused-for-ms
  "How long an older schema's index goes unused before it is removed."
  (* 24 60 60 1000))

(defn- delete-tree! [^File f]
  (doseq [^File x (reverse (file-seq f))] (.delete x)))

(defn- remove-unused-older-indexes!
  "Remove the indexes of older schema versions (and of the layout before
  them, the index directly in the home dir) that no daemon or editor uses
  and that nothing has written for a day."
  [home]
  (let [cutoff (- (System/currentTimeMillis) unused-for-ms)
        stale? (fn [lock-path files]
                 (and (not (lock/held? lock-path))
                      (every? #(< (.lastModified ^File %) cutoff) (filter #(.exists ^File %) files))))]
    (doseq [^File d (.listFiles (io/file home))
            :let [v (some-> (re-matches #"v(\d+)" (.getName d)) second parse-long)]
            :when (and v (.isDirectory d) (< v schema/version))
            :let [{:keys [daemon-lock clients-lock]} (home/paths home v)]
            :when (not (lock/in-use? clients-lock))
            ;; the index's files: probing the lock creates it anew
            :when (stale? daemon-lock (filter #(.startsWith (.getName ^File %) "index.db") (file-seq d)))]
      (delete-tree! d))
    (let [legacy (map #(io/file home %) ["index.db" "index.db-wal" "index.db-shm" "daemon.lock" "spawn.lock"
                                         "daemon.log" "daemon.log.1"])
          db (io/file home "index.db")]
      (when (and (.exists db) (stale? (str (io/file home "daemon.lock")) [db]))
        (run! #(.delete ^File %) legacy)))))

(def defaults
  "The daemon's timing, in ms: how often it looks for work, how long idle
  before it collects garbage and before it exits, how often it says it's
  alive, and how long it waits after the machine failed."
  {:poll-ms 150
   :gc-after-idle-ms (* 30 1000)
   :idle-exit-ms (* 10 60 1000)
   :heartbeat-ms 5000
   ;; after the machine failed (a full disk), before trying the queue again
   :retry-ms 5000})

(defn- pid [] (.pid (ProcessHandle/current)))

(defn- register! [c version]
  (let [now (System/currentTimeMillis)]
    (db/execute! c "INSERT INTO daemon (id, pid, version, started_at, heartbeat_at, stop_requested)
                    VALUES (1, ?, ?, ?, ?, 0)
                    ON CONFLICT (id) DO UPDATE SET pid = excluded.pid, version = excluded.version,
                      started_at = excluded.started_at, heartbeat_at = excluded.heartbeat_at,
                      stop_requested = 0"
                 (pid) version now now)))

(defn- beat! [c now]
  (db/execute! c "UPDATE daemon SET heartbeat_at = ? WHERE id = 1" now))

(defn- unregister! [c]
  (db/execute! c "DELETE FROM daemon WHERE id = 1 AND pid = ?" (pid)))

(defn- stop-requested? [c]
  (= 1 (db/query-value c "SELECT stop_requested FROM daemon WHERE id = 1")))

(defn- data-version [c] (db/query-value c "PRAGMA data_version"))

(defn- gc-requests
  "The ids of the garbage collections clients (clojure-lite-lsp gc) are waiting for."
  [c]
  (for [[k] (db/query c "SELECT key FROM meta WHERE key LIKE ?" (str schema/gc-request-prefix "%"))]
    (subs k (count schema/gc-request-prefix))))

(defn- collect-requested!
  "Collect garbage for the clients waiting (clojure-lite-lsp gc), once, answering each
  in meta. The batch in flight is written first: it may be about to link
  units nothing uses yet."
  [{:keys [c w] :as ix} requests]
  (indexer/drain! ix)
  (let [result (try (gc/collect! w {:sweep :always})
                    (catch Exception e {:error (ex-message e)}))]
    (db/with-tx c
      (doseq [r requests]
        (db/execute! c "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)" (schema/gc-result-key r) (pr-str result))
        (db/execute! c "DELETE FROM meta WHERE key = ?" (schema/gc-request-key r))))))

(defn- pending? [c] (some? (db/query-value c "SELECT 1 FROM pending LIMIT 1")))

(defn- now-ms [] (System/currentTimeMillis))

(defn- tick!
  "One turn of the work loop: [:done result] or [:next state]."
  [{:keys [c w] :as ix} {:keys [poll-ms gc-after-idle-ms idle-exit-ms heartbeat-ms retry-ms version]}
   {:keys [last-work collected? seen-version last-beat] :as state}]
  (let [now (now-ms)
        beat? (> (- now last-beat) heartbeat-ms)
        state (cond-> state beat? (assoc :last-beat now))
        dv (data-version c)]
    (when beat? (beat! c now))
    (if (stop-requested? c)
      [:done :stopped]
      (if-let [requests (seq (gc-requests c))]
        (do (collect-requested! ix requests)
            [:next (assoc state :last-work (now-ms) :collected? true :seen-version nil)])
        ;; something changed (or we just started): work the queue
        (let [worked (when (not= dv seen-version) (indexer/step! ix))]
          (cond
            (= :retry worked)
            (do (Thread/sleep (long retry-ms))
                [:next (assoc state :last-work (now-ms) :seen-version nil)])

            worked
            [:next (assoc state :last-work (now-ms) :collected? false :seen-version nil)]

            (and (not collected?) (> (- now last-work) gc-after-idle-ms))
            (do (try (gc/collect! w {})
                     (catch Exception e (log/warn "garbage collection failed:" (ex-message e))))
                [:next (assoc state :collected? true :seen-version (data-version c))])

            (> (- now last-work) idle-exit-ms)
            ;; a client that enqueued since sees this daemon as running (it
            ;; holds its lock until it's gone): leave its row first, then look
            ;; once more
            (do (unregister! c)
                (if (pending? c)
                  (do (register! c version)
                      [:next (assoc state :last-work now :seen-version nil)])
                  [:done :idle]))

            :else
            (do (Thread/sleep (long poll-ms))
                [:next (assoc state :seen-version dv)])))))))

(defn- work-loop!
  "Run until asked to stop or idle long enough. Returns :stopped or :idle.
  A failure (the database busy for too long, a full disk, an analysis out
  of memory on one huge file) is logged and the loop goes on after a
  pause: the daemon's work is only ever queued."
  [ix opts]
  (loop [state {:last-work (now-ms) :collected? true :seen-version nil :last-beat 0}]
    (let [[k v] (try (tick! ix opts state)
                     (catch InterruptedException e (throw e))
                     ;; Throwable, not Exception: errors too (above)
                     (catch Throwable e
                       (log/warn "daemon loop failed:" (ex-message e))
                       (Thread/sleep (long (:retry-ms opts)))
                       [:next (assoc state :seen-version nil)]))]
      (if (= :done k) v (recur v)))))

(def ^:private lock-patience-ms
  "How long a starting daemon keeps trying daemon.lock. Clients probe
  liveness by taking it for an instant, so failing once doesn't mean
  another daemon runs; a running daemon holds it for good."
  2000)

(defn- take-daemon-lock [path]
  (lock/poll #(lock/try-lock path) lock-patience-ms 10))

(defn serve!
  "Run the daemon in this process until it stops. Returns :stopped, :idle,
  or :already-running when another daemon holds the lock."
  [{:keys [home version] :or {version version/version} :as opts}]
  (let [{:keys [db daemon-lock dir]} (home/paths home)
        opts (merge defaults opts)]
    (.mkdirs (io/file dir))
    (if-let [held (take-daemon-lock daemon-lock)]
      (try
        (remove-unused-older-indexes! home)
        (with-open [^Closeable ix (indexer/indexer {:db-path db :home home})]
          (register! (:c ix) version)
          (try
            (work-loop! ix (assoc opts :version version))
            (finally
              (unregister! (:c ix)))))
        (finally
          (lock/release! held)))
      :already-running)))
