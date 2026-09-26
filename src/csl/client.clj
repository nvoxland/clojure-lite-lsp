(ns csl.client
  "The csl lsp side of the daemon lifecycle: make sure a daemon of this
  version is running, starting one if needed (DESIGN.md §6.3)."
  (:require
   [clojure.java.io :as io]
   [csl.daemon :as daemon]
   [csl.db :as db]
   [csl.lock :as lock]
   [csl.version :as version])
  (:import
   [java.lang ProcessBuilder$Redirect]))

(set! *warn-on-reflection* true)

(def ^:private timeout-ms 30000)

(defn request-stop!
  "Ask the running daemon to finish its batch and exit."
  [c]
  (db/execute! c "UPDATE daemon SET stop_requested = 1 WHERE id = 1"))

(defn- wait-until [pred]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond (pred) true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 20) (recur))
            :else false))))

(defn- running-version
  "The version of the daemon holding the lock. It takes the lock before it
  writes its row, so wait briefly for the row."
  [db-path]
  (with-open [c (db/open-client db-path)]
    (let [v (atom nil)]
      (wait-until #(reset! v (try (db/query-value c "SELECT version FROM daemon WHERE id = 1")
                                  (catch java.sql.SQLException _ nil))))
      @v)))

(defn daemon-command
  "How to start a daemon: this same native binary, or on the JVM (dev) this
  same classpath."
  []
  (if (System/getProperty "org.graalvm.nativeimage.imagecode")
    [(.orElseThrow (.command (.info (java.lang.ProcessHandle/current)))) "-Xmx768m" "index"]
    [(str (io/file (System/getProperty "java.home") "bin" "java"))
     "-Xmx768m" "--enable-native-access=ALL-UNNAMED" "-cp" (System/getProperty "java.class.path")
     "clojure.main" "-m" "csl.main" "index"]))

(defn spawn-daemon!
  "Start a detached daemon process logging to the home dir's daemon.log."
  [home]
  (let [{:keys [log]} (daemon/paths home)]
    (-> (ProcessBuilder. ^java.util.List (daemon-command))
        (.redirectInput ProcessBuilder$Redirect/PIPE)
        (.redirectErrorStream true)
        (.redirectOutput (ProcessBuilder$Redirect/appendTo (io/file log)))
        (doto (-> .environment (.put "CSL_HOME" (str home))))
        (.start)
        (-> .getOutputStream .close))))

(defn ensure-daemon!
  "Make sure a daemon of `version` is running. Returns :running or
  :spawned."
  [{:keys [home version spawn!] :or {version version/version}}]
  (let [{:keys [db daemon-lock spawn-lock]} (daemon/paths home)
        spawn! (or spawn! #(spawn-daemon! home))
        live? #(lock/held? daemon-lock)]
    (when (and (live?) (not= version (running-version db)))
      ;; another version: ask it to stop, and replace it
      (with-open [c (db/open-client db)] (request-stop! c))
      (wait-until #(not (live?))))
    (if (and (live?) (= version (running-version db)))
      :running
      (let [held (lock/lock! spawn-lock timeout-ms)]
        (try
          (if (live?)
            :running
            (do (spawn!)
                ;; the daemon takes its lock, then creates the schema, then
                ;; registers: its row means the index is ready to use
                (when-not (and (wait-until live?) (running-version db))
                  (throw (ex-info "The csl daemon did not start; see daemon.log" {:home home})))
                :spawned))
          (finally (lock/release! held)))))))
