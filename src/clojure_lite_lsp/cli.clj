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
   [java.io File]))

(set! *warn-on-reflection* true)

(def ^:private poll-ms 200)

(def ^:private gc-timeout-ms
  "How long `gc!` waits for the daemon to collect."
  (* 30 60 1000))

(def ^:private stop-timeout-ms
  "How long `stop!` waits for the daemon to finish its batch."
  60000)

(defn- revive-daemon!
  "Start a daemon again should it have gone while waiting on it (it
  crashed, or was replaced): a lock probe first, which is cheaper than
  `client/ensure-daemon!`'s database check."
  [opts]
  (when-not (lock/held? (:daemon-lock (home/paths (:home opts))))
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
                (revive-daemon! opts)
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
                                       (do (revive-daemon! opts) nil)))
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

(defn- fail!
  "Stop `query!` with `message` (exit 1)."
  [message]
  (throw (ex-info message {})))

(defn- query-root
  "The project a query is about: `project`, else the one `cwd` is in."
  [cwd project]
  (if project
    (if (.isDirectory (io/file project))
      (or (project-root project) (.getCanonicalPath (io/file project)))
      (fail! (str "Not a directory: " project)))
    (or (project-root cwd)
        (fail! (str "Not in a Clojure project: no " (str/join ", " classpath/build-files)
                    " in " cwd " or above (--project <dir> names one)")))))

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
        (let [root (query-root cwd project)
              {:keys [pending]} (when sync? (index! opts [root] (constantly nil) {:deadline-ms deadline-ms}))
              {:keys [db]} (home/paths (:home opts))
              note (when (seq pending)
                     (str "(clojure-lite-lsp is still indexing " root ": " (reduce + (vals pending))
                          " to go; answers may be incomplete)"))]
          (when-not (.isFile (io/file db))
            (fail! (str "No index yet: run clojure-lite-lsp index " root)))
          (with-open [c (db/open-reader db)]
            (let [p (or (db/query-value c "SELECT id FROM project WHERE root = ?" root)
                        (fail! (str root " isn't indexed yet: run clojure-lite-lsp index " root)))
                  ctx {:c c :p p :root root :home (:home opts) :cwd (.getCanonicalPath (io/file cwd))}
                  result (commands/run ctx args {:limit limit})]
              {:exit 0 :out (if json?
                              (json/generate-string (cond-> result note (assoc :note note)))
                              (str (some-> note (str "\n")) (commands/format-text ctx cmd result)))})))))
    (catch clojure.lang.ExceptionInfo e
      (if (:usage (ex-data e))
        {:exit 2 :out (str (ex-message e) "\n\n" (commands/help))}
        {:exit 1 :out (ex-message e)}))
    (catch Exception e
      {:exit 1 :out (str "Failed: " (or (ex-message e) (.getName (class e))))})))
