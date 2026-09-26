(ns csl.main
  "The `csl` command: `csl index` runs the daemon, `csl status` reports
  on it. (`csl lsp` comes with the LSP server.)"
  (:require
   [clojure.java.io :as io]
   [csl.daemon :as daemon]
   [csl.db :as db]
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
        (let [[pid v hb] (first (db/query c "SELECT pid, version, heartbeat_at FROM daemon WHERE id = 1"))]
          (println "Index:  " db (str "(" (quot (.length (io/file db)) 1048576) " MB)"))
          (println "Daemon: " (if pid (str "pid " pid ", version " v ", heartbeat "
                                           (quot (- (System/currentTimeMillis) hb) 1000) " s ago")
                                  "not running"))
          (doseq [[root pending] (db/query c "SELECT p.root, count(q.path) FROM project p
                                              LEFT JOIN pending q ON q.project_id = p.id GROUP BY p.id")]
            (println "Project:" root (str "(" pending " pending)"))))))))

(defn -main [& [cmd]]
  (case cmd
    "index" (do (daemon/run! {:home (home)})
                (shutdown-agents)
                (System/exit 0))
    "status" (status (home))
    "version" (println version/version)
    (do (println "Usage: csl index | status | version")
        (System/exit 1))))
