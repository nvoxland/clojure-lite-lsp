(ns clojure-lite-lsp.snapshot
  "Each project's current snapshot: its files and jars, and the derived
  `project_unit` table (every unit a project can see, with its precedence)
  that queries join through.

  Precedence (`ord`) is a classpath position: 0 for the project's own
  sources, then each jar or external directory's position on the classpath.
  Lower wins."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.writer :as writer]))

(set! *warn-on-reflection* true)

(defn ensure-project!
  "The id of the project at `root`, creating it if needed."
  [c root]
  (db/execute! c "INSERT INTO project (root, last_seen) VALUES (?, ?)
                  ON CONFLICT (root) DO UPDATE SET last_seen = excluded.last_seen"
               root (System/currentTimeMillis))
  (db/query-value c "SELECT id FROM project WHERE root = ?" root))

(defn- refresh-project-unit!
  "Recompute project `p`'s row for unit `u` from its files and jars."
  [c p u]
  (when u
    (if-let [ord (db/query-value c "SELECT min(ord) FROM (
                                      SELECT ord FROM project_file WHERE project_id = ? AND unit_id = ?
                                      UNION ALL
                                      SELECT pj.ord FROM project_jar pj
                                      JOIN jar_entry je ON je.jar_id = pj.jar_id
                                      WHERE pj.project_id = ? AND je.unit_id = ?)"
                                 p u p u)]
      (db/execute! c "INSERT INTO project_unit (project_id, unit_id, ord) VALUES (?, ?, ?)
                      ON CONFLICT (project_id, unit_id) DO UPDATE SET ord = excluded.ord"
                   p u ord)
      (db/execute! c "DELETE FROM project_unit WHERE project_id = ? AND unit_id = ?" p u))))

(defn- file-unit [c p path]
  (db/query-value c "SELECT unit_id FROM project_file WHERE project_id = ? AND path = ?" p path))

(defn set-file-unit!
  "Point project `p`'s file at `path` to unit `u` (nil: not indexed yet).
  `external?` marks a file from a classpath directory outside the project."
  [w p path u {:keys [ord external?]}]
  (writer/with-write-tx w
    (let [c (:c w)
          old (file-unit c p path)]
      (db/execute! c "INSERT INTO project_file (project_id, path, unit_id, external, ord) VALUES (?, ?, ?, ?, ?)
                      ON CONFLICT (project_id, path) DO UPDATE
                      SET unit_id = excluded.unit_id, external = excluded.external, ord = excluded.ord"
                   p path u (if external? 1 0) ord)
      (refresh-project-unit! c p old)
      (refresh-project-unit! c p u))))

(defn remove-file!
  "Forget project `p`'s file at `path`."
  [w p path]
  (writer/with-write-tx w
    (let [c (:c w)
          old (file-unit c p path)]
      (db/execute! c "DELETE FROM project_file WHERE project_id = ? AND path = ?" p path)
      (refresh-project-unit! c p old))))

(defn jar-id
  "The id of the jar with `jar-key`, if it has been written."
  [c {:keys [jar-hash config-hash kondo-version options-hash]}]
  (db/query-value c "SELECT id FROM jar WHERE jar_hash = ? AND config_hash = ? AND kondo_version = ? AND options_hash = ?"
                  jar-hash config-hash kondo-version options-hash))

(defn- java-class-entry?
  "An entry whose only content is Java class definitions."
  [[_ _ elements]]
  (and (seq elements) (every? #(= :java-class-def (:kind %)) elements)))

(defn write-jar!
  "Write a jar's entries in one transaction, unless a jar with the same key
  exists. `jar-key` has :jar-hash :config-hash :kondo-version :options-hash;
  `entries` is a seq of [entry-path unit-key elements]. Clojure entries
  become units; Java classes are recorded against the jar itself.
  Returns the jar id."
  [w jar-key entries]
  (writer/with-write-tx w
    (let [c (:c w)]
      (or (jar-id c jar-key)
          (let [{:keys [jar-hash config-hash kondo-version options-hash]} jar-key
                unit-entries (remove java-class-entry? entries)
                units (writer/write-units! w (map (fn [[_ k els]] [k els]) unit-entries))]
            (db/execute! c "INSERT INTO jar (jar_hash, config_hash, kondo_version, options_hash)
                            VALUES (?, ?, ?, ?)"
                         jar-hash config-hash kondo-version options-hash)
            (let [id (jar-id c jar-key)]
              (doseq [[[path] u] (map vector unit-entries units)]
                (db/execute! c "INSERT INTO jar_entry (jar_id, entry_path, unit_id) VALUES (?, ?, ?)"
                             id path u))
              (writer/write-java-classes! w id
                                          (for [[path _ els] entries
                                                el els
                                                :when (= :java-class-def (:kind el))]
                                            [(:name el) path]))
              id))))))

(defn set-project-jars!
  "Replace project `p`'s jars with `jars`, a seq of [ord path jar-id]
  (jar-id nil: not indexed yet), and recompute what it can see."
  [w p jars]
  (writer/with-write-tx w
    (let [c (:c w)]
      (db/execute! c "DELETE FROM project_jar WHERE project_id = ?" p)
      (doseq [[ord path jar-id] jars]
        (db/execute! c "INSERT INTO project_jar (project_id, ord, path, jar_id) VALUES (?, ?, ?, ?)"
                     p ord path jar-id))
      (db/execute! c "DELETE FROM project_unit WHERE project_id = ?" p)
      (db/execute! c "INSERT INTO project_unit (project_id, unit_id, ord)
                      SELECT ?, unit_id, min(ord) FROM (
                        SELECT unit_id, ord FROM project_file WHERE project_id = ? AND unit_id IS NOT NULL
                        UNION ALL
                        SELECT je.unit_id, pj.ord FROM project_jar pj
                        JOIN jar_entry je ON je.jar_id = pj.jar_id
                        WHERE pj.project_id = ?)
                      GROUP BY unit_id"
                   p p p))))

(defn link-jar!
  "Record that project `p`'s jar at classpath position `ord` is now
  indexed as `jar-id`, and make its units visible, without recomputing the
  project's other visibility."
  [w p ord jar-id]
  (writer/with-write-tx w
    (let [c (:c w)]
      (db/execute! c "UPDATE project_jar SET jar_id = ? WHERE project_id = ? AND ord = ?" jar-id p ord)
      (db/execute! c "INSERT INTO project_unit (project_id, unit_id, ord)
                      SELECT ?, unit_id, ? FROM jar_entry WHERE jar_id = ?
                      ON CONFLICT (project_id, unit_id) DO UPDATE SET ord = min(ord, excluded.ord)"
                   p ord jar-id))))

(defn file-paths
  "Project `p`'s files: {path {:unit-id :external? :ord}}."
  [c p]
  (into {} (map (fn [[path u ext ord]] [path {:unit-id u :external? (= 1 ext) :ord ord}]))
        (db/query c "SELECT path, unit_id, external, ord FROM project_file WHERE project_id = ?" p)))

(defn set-dep-file-unit!
  "Record that the extracted library file at `path` (from the jar with
  `jar-hash`) is fully analyzed as unit `u`."
  [w path jar-hash u]
  (writer/with-write-tx w
    (db/execute! (:c w) "INSERT INTO dep_file (path, jar_hash, unit_id) VALUES (?, ?, ?)
                         ON CONFLICT (path) DO UPDATE SET jar_hash = excluded.jar_hash, unit_id = excluded.unit_id"
                 path jar-hash u)))
