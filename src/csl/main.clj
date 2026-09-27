(ns csl.main
  "The `csl` command: `csl lsp` is the language server an editor runs,
  `csl index` the daemon it starts, `csl status` reports on them. For
  humans: `csl index <dir>...` indexes projects and waits, `csl gc`
  collects garbage now."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.cli :as cli]
   [csl.daemon :as daemon]
   [csl.db :as db]
   [csl.lock :as lock]
   [csl.lsp.server :as server]
   [csl.status :as status]
   [csl.version :as version])
  (:gen-class))

(defn home
  "The csl home dir: $CSL_HOME, else ~/.cache/clojure-sqlite-lsp."
  []
  (or (System/getenv "CSL_HOME")
      (str (io/file (System/getProperty "user.home") ".cache" "clojure-sqlite-lsp"))))

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
  "Usage: csl lsp | index [<project-dir>...] | gc | status | version")

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
