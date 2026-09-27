(ns clojure-lite-lsp.cli
  "Commands for humans: `clojure-lite-lsp index <dir>...` indexes projects and waits
  until they are done (e.g. from a hook that creates a worktree, so an
  editor opened on it finds its index ready), and `clojure-lite-lsp gc` collects
  garbage now. Both go through the daemon, the index's only writer."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]))

(set! *warn-on-reflection* true)

(def ^:private poll-ms 200)

(defn- keep-daemon!
  "Start a daemon again should it have gone (it crashed, or was replaced)."
  [opts]
  (when-not (lock/held? (:daemon-lock (daemon/paths (:home opts))))
    (client/ensure-daemon! opts)))

(defn index!
  "Index the projects at `dirs` and wait until they are done, calling
  `progress` with {root pending-count} as it goes. Returns {:files {root
  indexed-file-count}}. `opts` are clojure-lite-lsp.client/ensure-daemon!'s."
  [opts dirs progress]
  (doseq [d dirs]
    (when-not (.isDirectory (io/file d))
      (throw (ex-info (str "Not a directory: " d) {:dir d}))))
  (client/ensure-daemon! opts)
  (with-open [c (db/open-client (:db (daemon/paths (:home opts))))]
    (let [projects (into {} (for [d dirs
                                  :let [root (.getCanonicalPath (io/file d))]]
                              [root (snapshot/ensure-project! c root)]))]
      (doseq [p (vals projects)] (queue/enqueue! c p :sync "" 1))
      (loop []
        (let [pending (update-vals projects #(queue/pending-count c %))]
          (progress pending)
          (if (every? zero? (vals pending))
            {:files (update-vals projects #(db/query-value c "SELECT count(*) FROM project_file
                                                              WHERE project_id = ? AND unit_id IS NOT NULL" %))}
            (do (Thread/sleep (long poll-ms))
                (keep-daemon! opts)
                (recur))))))))

(defn gc!
  "Have the daemon collect garbage now (between batches, should it be
  indexing), and wait. Returns what was dropped: {:projects :jars
  :units}."
  [opts]
  (client/ensure-daemon! opts)
  (with-open [c (db/open-client (:db (daemon/paths (:home opts))))]
    (let [request (str (random-uuid))]
      (db/execute! c "INSERT INTO meta (key, value) VALUES (?, '')" (str "gc_request:" request))
      (loop []
        (if-let [result (db/query-value c "SELECT value FROM meta WHERE key = ?" (str "gc_result:" request))]
          (do (db/execute! c "DELETE FROM meta WHERE key = ?" (str "gc_result:" request))
              (edn/read-string result))
          (do (Thread/sleep (long poll-ms))
              (keep-daemon! opts)
              (recur)))))))
