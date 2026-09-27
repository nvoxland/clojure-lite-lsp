(ns clojure-lite-lsp.client
  "The clojure-lite-lsp lsp side of the daemon lifecycle: make sure a daemon of this
  version or a newer one is running, starting one if needed (DESIGN.md
  §6.3)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.version :as version])
  (:import
   [java.lang ProcessBuilder$Redirect]))

(set! *warn-on-reflection* true)

(def ^:private timeout-ms 30000)

(defn request-stop!
  "Ask the running daemon (only the one with `pid`, when given) to finish
  its batch and exit."
  ([c] (db/execute! c "UPDATE daemon SET stop_requested = 1 WHERE id = 1"))
  ([c pid] (db/execute! c "UPDATE daemon SET stop_requested = 1 WHERE id = 1 AND pid = ?" pid)))

(defn- wait-until [pred & [ms]]
  (let [deadline (+ (System/currentTimeMillis) (or ms timeout-ms))]
    (loop []
      (cond (pred) true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 20) (recur))
            :else false))))

(defn- running
  "{:pid :version} of the daemon holding the lock `daemon-lock`, or nil
  when there is none. A daemon takes the lock before it registers, and
  leaves its row before it lets go of the lock, so wait for one or the
  other."
  [db-path daemon-lock]
  (with-open [c (db/open-client db-path)]
    (let [r (atom nil)]
      (wait-until #(or (reset! r (try (first (db/query c "SELECT pid, version FROM daemon WHERE id = 1"))
                                      (catch java.sql.SQLException _ nil)))
                       (not (lock/held? daemon-lock))))
      (when-let [[pid version] @r]
        (when (lock/held? daemon-lock) {:pid pid :version version})))))

(defn- version-key [v]
  (let [[number qualifier] (str/split (str v) #"-" 2)]
    [(mapv parse-long (re-seq #"\d+" number)) (if qualifier 0 1)]))

(defn older?
  "Is version `a` older than `b`? Compared by number; a -SNAPSHOT (or any
  qualifier) is older than its release."
  [a b]
  (let [[na qa] (version-key a)
        [nb qb] (version-key b)
        width (max (count na) (count nb))
        pad #(into % (repeat (- width (count %)) 0))]
    (neg? (compare [(pad na) qa] [(pad nb) qb]))))

(defn daemon-command
  "How to start a daemon: this same native binary, or on the JVM (dev) this
  same classpath."
  []
  (if (System/getProperty "org.graalvm.nativeimage.imagecode")
    [(.orElseThrow (.command (.info (java.lang.ProcessHandle/current)))) "-Xmx768m" "index"]
    [(str (io/file (System/getProperty "java.home") "bin" "java"))
     "-Xmx768m" "--enable-native-access=ALL-UNNAMED" "-cp" (System/getProperty "java.class.path")
     "clojure.main" "-m" "clojure-lite-lsp.main" "index"]))

(def ^:private max-log-bytes (* 5 1024 1024))

(defn rotate-log!
  "Start a new log at `path` once it is over 5 MB, keeping the previous
  one as path.1."
  [path]
  (let [f (io/file path)]
    (when (> (.length f) max-log-bytes)
      (let [old (io/file (str path ".1"))]
        (.delete old)
        (.renameTo f old)))))

(defn spawn-daemon!
  "Start a detached daemon process logging to the home dir's daemon.log."
  [home]
  (let [{:keys [log]} (daemon/paths home)]
    (rotate-log! log)
    (-> (ProcessBuilder. ^java.util.List (daemon-command))
        (.redirectInput ProcessBuilder$Redirect/PIPE)
        (.redirectErrorStream true)
        (.redirectOutput (ProcessBuilder$Redirect/appendTo (io/file log)))
        (doto (-> .environment (.put "CLOJURE_LITE_LSP_HOME" (str home))))
        (.start)
        (-> .getOutputStream .close))))

(defn ensure-daemon!
  "Make sure a daemon of `version`, or a newer one, is running. Returns
  :running or :spawned.

  Only an older daemon is replaced: with two editors running different
  versions, the older one uses the newer daemon rather than the two
  replacing each other's over and over. (Versions with different schemas
  don't meet: each has its own index, `daemon/paths`.)"
  [{:keys [home version spawn!] :or {version version/version} :as opts}]
  (let [{:keys [db daemon-lock spawn-lock dir]} (daemon/paths home)
        _ (.mkdirs (io/file dir))
        spawn! (or spawn! #(spawn-daemon! home))
        live? #(lock/held? daemon-lock)
        current #(when-let [{v :version :as d} (running db daemon-lock)]
                   (when-not (older? v version) d))]
    (if-let [{:keys [pid] v :version} (when-let [d (running db daemon-lock)] (when (older? (:version d) version) d))]
      ;; an older one: ask it to stop. Mid-way through a long step it stays
      ;; until the step ends; meanwhile it still serves, and a later call
      ;; starts this version once it's gone (rather than every enqueue
      ;; waiting for it)
      (do (with-open [c (db/open-client db)] (request-stop! c pid))
          (if (wait-until #(not (live?)) 2000)
            (ensure-daemon! opts)
            :running))
    (if (current)
      :running
      (let [held (lock/lock! spawn-lock timeout-ms)]
        (try
          (if (current)
            :running
            (do (spawn!)
                ;; the daemon takes its lock, then creates the schema, then
                ;; registers: its row means the index is ready to use
                (when-not (and (wait-until live?) (running db daemon-lock))
                  (throw (ex-info "The clojure-lite-lsp daemon did not start; see daemon.log" {:home home})))
                :spawned))
          (finally (lock/release! held))))))))
