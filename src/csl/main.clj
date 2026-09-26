(ns csl.main
  "The `csl` command: `csl lsp` is the language server an editor runs,
  `csl index` the daemon it starts, `csl status` reports on them."
  (:require
   [clojure.java.io :as io]
   [csl.daemon :as daemon]
   [csl.db :as db]
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
  (let [{:keys [db]} (daemon/paths h)]
    (if-not (.isFile (io/file db))
      (println "No index at" db)
      (with-open [c (db/open-reader db)]
        ;; the WAL holds recent writes until they are checkpointed
        (status/print! (status/data c)
                       (quot (+ (.length (io/file db)) (.length (io/file (str db "-wal")))) 1048576))))))

(defn -main [& [cmd]]
  (case cmd
    ;; stdout is the LSP connection: nothing else may print there
    "lsp" (let [code (server/run! {:in System/in :out System/out :home (home)})]
            (shutdown-agents)
            (System/exit code))
    "index" (do (daemon/run! {:home (home)})
                (shutdown-agents)
                (System/exit 0))
    "status" (status (home))
    "version" (println version/version)
    (do (println "Usage: csl lsp | index | status | version")
        (System/exit 1))))
