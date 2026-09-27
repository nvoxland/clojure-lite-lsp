(ns clojure-lite-lsp.main
  "The `clojure-lite-lsp` command: `clojure-lite-lsp lsp` is the language server an editor runs,
  `clojure-lite-lsp index` the daemon it starts, `clojure-lite-lsp status` reports on them. For
  humans: `clojure-lite-lsp index <dir>...` indexes projects and waits, `clojure-lite-lsp gc`
  collects garbage now."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.lsp.server :as server]
   [clojure-lite-lsp.status :as status]
   [clojure-lite-lsp.version :as version])
  (:gen-class))

(defn home
  "The clojure-lite-lsp home dir: $CLOJURE_LITE_LSP_HOME, else ~/.cache/clojure-lite-lsp."
  []
  (or (System/getenv "CLOJURE_LITE_LSP_HOME")
      (str (io/file (System/getProperty "user.home") ".cache" "clojure-lite-lsp"))))

(defn- status [h]
  (let [{:keys [db daemon-lock]} (daemon/paths h)]
    (if-not (.isFile (io/file db))
      (println "No index at" db)
      (with-open [c (db/open-reader db)]
        (try
          ;; the WAL holds recent writes until they are checkpointed
          (status/print! (status/data c {:daemon-alive? (lock/held? daemon-lock)})
                         (quot (+ (.length (io/file db)) (.length (io/file (str db "-wal")))) 1048576))
          (catch java.sql.SQLException _
            (println "The index at" db "is still being created")))))))

(def ^:private usage
  "Usage: clojure-lite-lsp lsp | index [<project-dir>...] | gc | status | version")

(defn- index-projects [dirs]
  (let [shown (atom nil)
        {:keys [files]} (cli/index! {:home (home)} dirs
                                    (fn [pending]
                                      (let [line (str/join ", " (for [[root n] pending] (str root ": " n " to do")))]
                                        (when (not= line @shown)
                                          (reset! shown line)
                                          (binding [*out* *err*] (println line))))))]
    (doseq [[root n] files] (println "Indexed" root (str "(" n " files)")))))

(defn- gc []
  (let [{:keys [projects jars units error]} (cli/gc! {:home (home)})]
    (if error
      (do (println "Garbage collection failed:" error) (System/exit 1))
      (println "Dropped" projects "project(s)," jars "jar(s)," units "analyzed file(s)."))))

(defn -main [& [cmd & args]]
  (case cmd
    ;; stdout is the LSP connection: nothing else may print there
    "lsp" (let [code (server/run! {:in System/in :out System/out :home (home)})]
            (shutdown-agents)
            (System/exit code))
    "index" (do (if (seq args)
                  (try (index-projects args)
                       (catch clojure.lang.ExceptionInfo e
                         (println (ex-message e))
                         (System/exit 1)))
                  (daemon/run! {:home (home)}))
                (shutdown-agents)
                (System/exit 0))
    "gc" (do (gc) (shutdown-agents) (System/exit 0))
    "status" (status (home))
    "version" (println version/version)
    (do (println usage)
        (System/exit 1))))
