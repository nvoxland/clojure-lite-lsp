(ns clojure-lite-lsp.cli
  "Commands for humans: `clojure-lite-lsp index <dir>...` indexes projects and waits
  until they are done (e.g. from a hook that creates a worktree, so an
  editor opened on it finds its index ready), and `clojure-lite-lsp gc` collects
  garbage now. Both go through the daemon, the index's only writer."
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.commands :as commands]
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

;;;; query

(def ^:private project-markers ["deps.edn" "project.clj" "bb.edn" ".clojure-lite-lsp.edn"])

(defn project-root
  "The project `dir` is in: the nearest directory, `dir` or above, with a
  build file; else `dir` itself."
  [dir]
  (let [start (.getCanonicalFile (io/file dir))
        ^java.io.File found (or (first (filter (fn [^java.io.File d] (some #(.isFile (io/file d ^String %)) project-markers))
                                               (take-while some? (iterate #(.getParentFile ^java.io.File %) start))))
                                start)]
    (.getPath found)))

(defn- parse-query-args
  "{:args [command argument] :json? :sync? :project}."
  [args]
  (loop [[a & more] args acc {:args [] :sync? true}]
    (cond
      (nil? a) acc
      (= "--json" a) (recur more (assoc acc :json? true))
      (= "--no-sync" a) (recur more (assoc acc :sync? false))
      (= "--project" a) (recur (rest more) (assoc acc :project (first more)))
      :else (recur more (update acc :args conj a)))))

(defn query!
  "Run `clojure-lite-lsp query` with `args`, in directory `cwd`: {:exit
  :out}. The project is brought up to date first (unless --no-sync), so
  what an agent just edited is answered. `opts` are
  clojure-lite-lsp.client/ensure-daemon!'s."
  [opts args {:keys [cwd]}]
  (let [{:keys [args json? sync? project]} (parse-query-args args)
        [cmd] args]
    (if (or (nil? cmd) (#{"help" "--help" "-h"} cmd))
      {:exit 0 :out (commands/help)}
      (try
        (let [root (project-root (or project cwd))
              _ (when sync? (index! opts [root] (fn [_])))
              {:keys [db]} (daemon/paths (:home opts))]
          (if-not (.isFile (io/file db))
            {:exit 1 :out (str "No index yet: run clojure-lite-lsp index " root)}
            (with-open [c (db/open-reader db)]
              (if-let [p (db/query-value c "SELECT id FROM project WHERE root = ?" root)]
                (let [ctx {:c c :p p :root root :home (:home opts) :cwd (.getCanonicalPath (io/file cwd))}
                      result (commands/run ctx args)]
                  {:exit 0 :out (if json? (json/generate-string result) (commands/format-text ctx cmd result))})
                {:exit 1 :out (str root " isn't indexed yet: run clojure-lite-lsp index " root)}))))
        (catch clojure.lang.ExceptionInfo e
          {:exit 2 :out (str (ex-message e) "\n\n" (commands/help))})))))
