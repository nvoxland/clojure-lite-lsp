(ns clojure-lite-lsp.main
  "The `clojure-lite-lsp` command. For editors and agents: `lsp`, the
  language server, and `mcp`, the query commands as an MCP server. The
  indexer: `index` without arguments (started by the others). For people
  and scripts: `query`, `index <dir>...`, `setup`, `gc`, `stop`, `status`
  and `version` (clojure-lite-lsp.cli)."
  (:require
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.lsp.server :as server]
   [clojure-lite-lsp.mcp :as mcp]
   [clojure-lite-lsp.setup :as setup]
   [clojure-lite-lsp.status :as status]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [clojure.lang ExceptionInfo]
   [java.sql SQLException])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- print-err [& xs]
  (binding [*out* *err*]
    (apply println xs)))

(defn- print-status! [h]
  (let [{:keys [db daemon-lock]} (home/paths h)]
    (if-not (.isFile (io/file db))
      (println "No index at" db)
      (with-open [c (db/open-reader db)]
        (try
          ;; the WAL holds recent writes until they are checkpointed
          (status/print! (status/data c {:daemon-alive? (lock/held? daemon-lock)})
                         (quot (+ (.length (io/file db)) (.length (io/file (str db "-wal")))) 1048576))
          (catch SQLException _
            (println "The index at" db "is still being created")))))))

(def ^:private usage
  (str "Usage: clojure-lite-lsp <command>\n\n"
       "  query <command> <arg>        look things up: definitions, references, callers, ...\n"
       "                               (clojure-lite-lsp query lists them)\n"
       "  index [<project-dir>...]     index projects and wait; without dirs, run the indexer\n"
       "  lsp                          the language server, for editors and agents (stdio)\n"
       "  mcp                          the query commands as an MCP server, for agents (stdio)\n"
       "  setup --agent <agent> [dir]  make a project ready for a coding agent: "
       (str/join ", " (sort (keys setup/agents))) "\n"
       "  gc                           collect garbage in the index now\n"
       "  stop                         stop the indexer (it restarts when needed)\n"
       "  status                       the indexer, the index and its projects\n"
       "  version"))

(defn- index-projects!
  "Index the projects at `dirs`, showing progress on stderr."
  [dirs]
  (let [shown (atom nil)
        {:keys [files]} (cli/index! {:home (home/dir)} dirs
                                    (fn [pending]
                                      (let [line (str/join ", " (for [[root n] pending] (str root ": " n " to do")))]
                                        (when (not= line @shown)
                                          (reset! shown line)
                                          (print-err line)))))]
    (doseq [[root n] files] (println "Indexed" root (str "(" n " files)")))))

(defn- gc! []
  (let [{:keys [projects jars units error]} (cli/gc! {:home (home/dir)})]
    (if error
      (do (print-err "Garbage collection failed:" error) 1)
      (do (println "Dropped" projects "project(s)," jars "jar(s)," units "analyzed file(s).") 0))))

(defn- setup-project! [args]
  (let [{:keys [agents dir index?]} (setup/parse-args args)
        dir (or dir (System/getProperty "user.dir"))]
    (when (empty? agents)
      (throw (ex-info (str "Usage: clojure-lite-lsp setup --agent <" (str/join "|" (sort (keys setup/agents))) "> [dir] [--no-index]")
                      {:usage true})))
    (doseq [[i agent] (map-indexed vector agents)
            :let [{:keys [wrote notes]} (setup/setup! {:agent agent :dir dir :home (home/dir)
                                                       ;; once, after the last agent
                                                       :index! (when (and index? (= i (dec (count agents))))
                                                                 #(index-projects! [%]))})]]
      (doseq [path wrote] (println "Wrote" path))
      (doseq [note notes] (println note)))))

(defn- run-command
  "Run `cmd` with `args`: its exit code."
  [cmd args]
  (try
    (case cmd
      ;; stdout is the LSP connection: nothing else may print there
      "lsp" (server/serve! {:in System/in :out System/out :home (home/dir)})
      "index" (do (if (seq args) (index-projects! args) (daemon/serve! {:home (home/dir)})) 0)
      "gc" (gc!)
      "stop" (do (println (case (cli/stop! {:home (home/dir)})
                            :stopped "Stopped the indexer."
                            :not-running "The indexer isn't running."))
                 0)
      "setup" (do (setup-project! args) 0)
      "query" (let [{:keys [exit out]} (cli/query! {:home (home/dir)} args {:cwd (System/getProperty "user.dir")})]
                (println out)
                exit)
      ;; stdout is the MCP connection
      "mcp" (do (mcp/serve! {:in System/in :out System/out :opts {:home (home/dir)}
                             :cwd (System/getProperty "user.dir")})
                0)
      "status" (do (print-status! (home/dir)) 0)
      "version" (do (println version/version) 0)
      (do (print-err usage) 1))
    (catch ExceptionInfo e
      (print-err (ex-message e))
      (if (:usage (ex-data e)) 2 1))))

(defn -main [& [cmd & args]]
  (let [code (try
               (run-command cmd args)
               (catch Throwable t
                 (print-err "Failed:" (or (ex-message t) (str t)))
                 1))]
    (shutdown-agents)
    (System/exit code)))
