(ns clojure-lite-lsp.cli
  "Commands for humans: `clojure-lite-lsp index <dir>...` indexes projects and waits
  until they are done (e.g. from a hook that creates a worktree, so an
  editor opened on it finds its index ready), and `clojure-lite-lsp gc` collects
  garbage now. Both go through the daemon, the index's only writer."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

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
  indexed-file-count} :pending {root count still queued}}: past
  `deadline-ms`, it returns before they're done. `opts` are
  clojure-lite-lsp.client/ensure-daemon!'s."
  [opts dirs progress & [{:keys [deadline-ms]}]]
  (doseq [d dirs]
    (when-not (.isDirectory (io/file d))
      (throw (ex-info (str "Not a directory: " d) {:dir d}))))
  (client/ensure-daemon! opts)
  (with-open [c (db/open-client (:db (daemon/paths (:home opts))))]
    (let [projects (into {} (for [d dirs
                                  :let [root (.getCanonicalPath (io/file d))]]
                              [root (snapshot/ensure-project! c root)]))]
      (doseq [p (vals projects)] (queue/enqueue! c p :sync "" 1))
      (loop [deadline (when deadline-ms (+ (System/currentTimeMillis) deadline-ms))]
        (let [pending (update-vals projects #(queue/pending-count c %))]
          (progress pending)
          (if (or (every? zero? (vals pending))
                  (and deadline (> (System/currentTimeMillis) deadline)))
            {:files (update-vals projects #(db/query-value c "SELECT count(*) FROM project_file
                                                              WHERE project_id = ? AND unit_id IS NOT NULL" %))
             :pending (into {} (filter (comp pos? val)) pending)}
            (do (Thread/sleep (long poll-ms))
                (keep-daemon! opts)
                (recur deadline))))))))

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

(defn stop!
  "Stop the indexer, once it finishes its batch: :stopped, or
  :not-running. Editors and queries start it again when they need it."
  [{:keys [home]}]
  (let [{:keys [db daemon-lock]} (daemon/paths home)]
    (if-not (lock/held? daemon-lock)
      :not-running
      (with-open [c (db/open-client db)]
        (client/request-stop! c)
        (loop [n 0]
          (cond (not (lock/held? daemon-lock)) :stopped
                (< n 3000) (do (Thread/sleep 20) (recur (inc n)))
                :else (throw (ex-info "The indexer didn't stop within a minute" {}))))))))

;;;; query

(def ^:private project-markers ["deps.edn" "project.clj" "bb.edn" ".clojure-lite-lsp.edn"])

(defn project-root
  "The project `dir` is in: the nearest directory, `dir` or above, with a
  build file, or nil."
  [dir]
  (let [start (.getCanonicalFile (io/file dir))]
    (some-> ^java.io.File (first (filter (fn [^java.io.File d] (some #(.isFile (io/file d ^String %)) project-markers))
                                         (take-while some? (iterate #(.getParentFile ^java.io.File %) start))))
            .getPath)))

(defn- parse-query-args
  "{:args [command argument] :json? :sync? :project :limit}."
  [args]
  (loop [[a & more] args acc {:args [] :sync? true :limit 200}]
    (cond
      (nil? a) acc
      (= "--json" a) (recur more (assoc acc :json? true))
      (= "--no-sync" a) (recur more (assoc acc :sync? false))
      (= "--project" a) (recur (rest more) (assoc acc :project (first more)))
      (= "--limit" a) (recur (rest more)
                             (assoc acc :limit (or (parse-long (str (first more)))
                                                   (throw (ex-info "--limit takes a number" {:usage true})))))
      (str/starts-with? a "--") (throw (ex-info (str "Unknown option: " a) {:usage true}))
      :else (recur more (update acc :args conj a)))))

(defn query!
  "Run `clojure-lite-lsp query` with `args`, in directory `cwd`: {:exit
  :out}. The project is brought up to date first (unless --no-sync), so
  what an agent just edited is answered; with `deadline-ms`, only for
  that long, saying so when indexing goes on. `opts` are
  clojure-lite-lsp.client/ensure-daemon!'s. Exit 2 is a usage error, 1 a
  failure."
  [opts args {:keys [cwd deadline-ms]}]
  (try
    (let [{:keys [args json? sync? project limit]} (parse-query-args args)
          [cmd] args]
      (if (or (nil? cmd) (#{"help" "--help" "-h"} cmd))
        {:exit 0 :out (commands/help)}
        (let [root (if project
                     (do (when-not (.isDirectory (io/file project))
                           (throw (ex-info (str "Not a directory: " project) {})))
                         (or (project-root project) (.getCanonicalPath (io/file project))))
                     (project-root cwd))]
          (if-not root
            {:exit 1 :out (str "Not in a Clojure project: no deps.edn, project.clj, bb.edn or "
                               ".clojure-lite-lsp.edn in " cwd " or above (--project <dir> names one)")}
            (let [{:keys [pending]} (when sync? (index! opts [root] (fn [_]) {:deadline-ms deadline-ms}))
                  {:keys [db]} (daemon/paths (:home opts))
                  note (when (seq pending)
                         (str "(clojure-lite-lsp is still indexing " root ": " (reduce + (vals pending))
                              " to go; answers may be incomplete)\n"))]
              (if-not (.isFile (io/file db))
                {:exit 1 :out (str "No index yet: run clojure-lite-lsp index " root)}
                (with-open [c (db/open-reader db)]
                  (if-let [p (db/query-value c "SELECT id FROM project WHERE root = ?" root)]
                    (let [ctx {:c c :p p :root root :home (:home opts) :cwd (.getCanonicalPath (io/file cwd))}
                          result (commands/run ctx args {:limit limit})]
                      {:exit 0 :out (if json?
                                      (json/generate-string (cond-> result note (assoc :note (str/trim note))))
                                      (str note (commands/format-text ctx cmd result)))})
                    {:exit 1 :out (str root " isn't indexed yet: run clojure-lite-lsp index " root)}))))))))
    (catch clojure.lang.ExceptionInfo e
      (if (:usage (ex-data e))
        {:exit 2 :out (str (ex-message e) "\n\n" (commands/help))}
        {:exit 1 :out (ex-message e)}))
    (catch Exception e
      {:exit 1 :out (str "Failed: " (or (ex-message e) (.getName (class e))))})))
