(ns csl.status
  "What `csl status` reports: the daemon, the index, and for each project
  its files and jars, including how many of its jars other projects use
  too (analysis is shared: a jar is analyzed once, ever)."
  (:require
   [csl.db :as db]))

(defn- project-data [c [p root last-seen]]
  (let [jar-ids (mapv first (db/query c "SELECT jar_id FROM project_jar WHERE project_id = ? AND jar_id IS NOT NULL" p))]
    {:id p :root root :last-seen last-seen
     :files (db/query-value c "SELECT count(*) FROM project_file WHERE project_id = ?" p)
     :files-indexed (db/query-value c "SELECT count(*) FROM project_file WHERE project_id = ? AND unit_id IS NOT NULL" p)
     :jars (db/query-value c "SELECT count(*) FROM project_jar WHERE project_id = ?" p)
     :jar-ids jar-ids
     :jars-shared (db/query-value c "SELECT count(DISTINCT pj.jar_id) FROM project_jar pj
                                     WHERE pj.project_id = ? AND pj.jar_id IN
                                       (SELECT jar_id FROM project_jar WHERE project_id <> ?)" p p)
     :pending (db/query-value c "SELECT count(*) FROM pending WHERE project_id = ?" p)}))

(defn data
  "The status of the index on connection `c`. `daemon-alive?`: whether a
  daemon holds its lock (its row outlives a crash)."
  ([c] (data c {:daemon-alive? true}))
  ([c {:keys [daemon-alive?]}]
  (let [[pid version heartbeat] (first (db/query c "SELECT pid, version, heartbeat_at FROM daemon WHERE id = 1"))]
    {:daemon (when (and pid daemon-alive?) {:pid pid :version version
                        :heartbeat-age-s (quot (- (System/currentTimeMillis) heartbeat) 1000)})
     :index {:units (db/query-value c "SELECT count(*) FROM unit")
             :jars (db/query-value c "SELECT count(*) FROM jar")
             :opened-library-files (db/query-value c "SELECT count(*) FROM dep_file")}
     :projects (mapv #(project-data c %) (db/query c "SELECT id, root, last_seen FROM project ORDER BY root"))})))

(defn print!
  "Print status `data` for people. `file-mb` is the index's size on disk."
  ([data] (print! data nil))
  ([{:keys [daemon index projects]} file-mb]
   (println "Daemon: " (if daemon
                         (str "pid " (:pid daemon) ", version " (:version daemon)
                              ", heartbeat " (:heartbeat-age-s daemon) " s ago")
                         "not running"))
   (println "Index:  " (str (when file-mb (str file-mb " MB, "))
                            (:units index) " analyzed files, " (:jars index) " jars"
                            (when (pos? (:opened-library-files index))
                              (str ", " (:opened-library-files index) " opened library files"))))
   (doseq [{:keys [root files files-indexed jars jars-shared pending]} projects]
     (println)
     (println root)
     (println (str "  files: " files-indexed "/" files " indexed"
                   (when (pos? pending) (str ", " pending " pending"))))
     (println (str "  jars: " jars " (" jars-shared " shared with other projects)")))))
