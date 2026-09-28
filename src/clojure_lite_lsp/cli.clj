(ns clojure-lite-lsp.cli
  "The commands besides the servers: `index` indexes projects and waits
  until they are done (e.g. from a hook that creates a worktree, so an
  editor opened on it finds its index ready), `gc` collects garbage now,
  `stop` stops the indexer, and `query` answers the query commands
  (clojure-lite-lsp.commands). All go through the daemon, the index's only
  writer."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.schema :as schema]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [clojure.lang ExceptionInfo]
   [java.io File]))

(set! *warn-on-reflection* true)

(def ^:private poll-ms 200)

(def ^:private gc-timeout-ms
  "How long `gc!` waits for the daemon to collect."
  (* 30 60 1000))

(def ^:private stop-timeout-ms
  "How long `stop!` waits for the daemon to finish its batch."
  60000)

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
  (with-open [c (db/open-client (:db (home/paths (:home opts))))]
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
                (client/ensure-daemon-alive! opts)
                (recur deadline))))))))

(defn gc!
  "Have the daemon collect garbage now (between batches, should it be
  indexing), and wait. Returns what was dropped: {:projects :jars
  :units}."
  [opts]
  (client/ensure-daemon! opts)
  (with-open [c (db/open-client (:db (home/paths (:home opts))))]
    (let [request (str (random-uuid))
          answered #(db/query-value c "SELECT value FROM meta WHERE key = ?" (schema/gc-result-key request))]
      (db/execute! c "INSERT INTO meta (key, value) VALUES (?, '')" (schema/gc-request-key request))
      (if-let [result (lock/poll (fn []
                                   (or (answered)
                                       (do (client/ensure-daemon-alive! opts) nil)))
                                 gc-timeout-ms poll-ms)]
        (do (db/execute! c "DELETE FROM meta WHERE key = ?" (schema/gc-result-key request))
            (edn/read-string result))
        (throw (ex-info "The indexer didn't collect garbage within 30 minutes" {}))))))

(defn stop!
  "Stop the indexer, once it finishes its batch: :stopped, or
  :not-running. Editors and queries start it again when they need it."
  [{:keys [home]}]
  (let [{:keys [db daemon-lock]} (home/paths home)]
    (if-not (lock/held? daemon-lock)
      :not-running
      (do (with-open [c (db/open-client db)]
            (client/request-stop! c))
          (if (lock/poll #(not (lock/held? daemon-lock)) stop-timeout-ms)
            :stopped
            (throw (ex-info "The indexer didn't stop within a minute" {})))))))

;;;; query

(defn project-root
  "The project `dir` is in: the nearest directory, `dir` or above, with a
  build file, or nil."
  [dir]
  (->> (iterate #(.getParentFile ^File %) (.getCanonicalFile (io/file dir)))
       (take-while some?)
       (some (fn [^File d]
               (when (some #(.isFile (io/file d ^String %)) classpath/build-files)
                 (.getPath d))))))

(defn- parse-query-args
  "`query`'s command line as a request for `answer`."
  [args]
  (loop [[a & more] args request {:args []}]
    (cond
      (nil? a) request
      (= "--json" a) (recur more (assoc request :json? true))
      (= "--no-sync" a) (recur more (assoc request :sync? false))
      (= "--project" a) (recur (rest more) (assoc request :project (first more)))
      (= "--limit" a) (recur (rest more)
                             (assoc request :limit (or (parse-long (str (first more)))
                                                       (throw (ex-info "--limit takes a number" {:usage true})))))
      (str/starts-with? a "--") (throw (ex-info (str "Unknown option: " a) {:usage true}))
      :else (recur more (update request :args conj a)))))

(defn- fail
  "Stop answering with `message` (exit 1)."
  [message]
  (throw (ex-info message {})))

(defn- query-root
  "The project a query is about: `project`, else the one `cwd` is in."
  [cwd project]
  (if project
    (if (.isDirectory (io/file project))
      (or (project-root project) (.getCanonicalPath (io/file project)))
      (fail (str "Not a directory: " project)))
    (or (project-root cwd)
        (fail (str "Not in a Clojure project: no " (str/join ", " classpath/build-files)
                   " in " cwd " or above (--project <dir> names one)")))))

(defn- result-of
  "The {:exit :out} of (f), or of its failure: exit 2 for a usage error
  (with the usage), 1 for anything else."
  [f]
  (try
    (f)
    (catch ExceptionInfo e
      (if (:usage (ex-data e))
        {:exit 2 :out (str (ex-message e) "\n\n" (commands/help))}
        {:exit 1 :out (ex-message e)}))
    (catch Exception e
      {:exit 1 :out (str "Failed: " (or (ex-message e) (.getName (class e))))})))

(defn- run-query
  "Run command `cmd` with `arg` in project `root`, as a result."
  [opts {:keys [args json? limit]} root cwd note]
  (let [{:keys [db]} (home/paths (:home opts))
        [cmd] args]
    (when-not (.isFile (io/file db))
      (fail (str "No index yet: run clojure-lite-lsp index " root)))
    (with-open [c (db/open-reader db)]
      (let [p (or (db/query-value c "SELECT id FROM project WHERE root = ?" root)
                  (fail (str root " isn't indexed yet: run clojure-lite-lsp index " root)))
            ctx {:c c :p p :root root :home (:home opts) :cwd (.getCanonicalPath (io/file cwd))}
            result (commands/run ctx args {:limit limit})]
        {:exit 0 :out (if json?
                        (json/generate-string (cond-> result note (assoc :note note)))
                        (str (some-> note (str "\n")) (commands/format-text ctx cmd result)))}))))

(defn answer
  "Answer a query `request`, {:args [command argument] :json? :sync?
  :project :limit}, asked in directory `cwd`: {:exit :out}. The project is
  brought up to date first (unless :sync? is false), so what an agent just
  edited is answered; with `deadline-ms`, only for that long, saying so
  when indexing goes on. `opts` are clojure-lite-lsp.client/ensure-daemon!'s.
  Exit 2 is a usage error, 1 a failure."
  [opts {:keys [args sync? project] :or {sync? true} :as request} {:keys [cwd deadline-ms]}]
  (result-of
   (fn []
     (if (or (empty? args) (#{"help" "--help" "-h"} (first args)))
       {:exit 0 :out (commands/help)}
       (let [root (query-root cwd project)
             {:keys [pending]} (when sync? (index! opts [root] (constantly nil) {:deadline-ms deadline-ms}))
             note (when (seq pending)
                    (str "(clojure-lite-lsp is still indexing " root ": " (reduce + (vals pending))
                         " to go; answers may be incomplete)"))]
         (run-query opts (merge {:limit 200} request) root cwd note))))))

(defn query!
  "Run `clojure-lite-lsp query` with command-line `args` (`answer`)."
  [opts args context]
  (result-of #(answer opts (parse-query-args args) context)))
