(ns csl.gc
  "Garbage collection of the index (DESIGN.md §6.7). The index only grows
  as files change and worktrees come and go; the daemon collects when its
  queue is empty, on its own loop thread, so collection never races
  indexing.

  - projects not seen for a while are dropped with their snapshots (this
    is what reclaims deleted worktrees)
  - a unit is live while some project sees it (project_unit); dead units
    lose every row
  - a jar is live while some project's classpath has it
  - workspace-symbol entries go when nothing defines the name any more"
  (:require
   [clojure.string :as str]
   [csl.db :as db]
   [csl.kinds :as kinds]
   [csl.writer :as writer]))

(def default-project-max-age-ms (* 1000 60 60 24 30))

(def ^:private batch 500)

(defn- in [n] (str "(" (str/join "," (repeat n "?")) ")"))

(defn- drop-stale-projects! [c max-age-ms]
  (let [ids (map first (db/query c "SELECT id FROM project WHERE last_seen < ?"
                                 (- (System/currentTimeMillis) max-age-ms)))]
    (doseq [p ids
            t ["project_file" "project_jar" "project_unit" "pending" "classpath_memo"]]
      (db/execute! c (str "DELETE FROM " t " WHERE project_id = ?") p))
    (doseq [p ids] (db/execute! c "DELETE FROM project WHERE id = ?" p))
    (count ids)))

(defn- delete-units! [c us]
  (let [q (in (count us))
        usage-kinds (str/join "," (map kinds/code kinds/usage-kinds))
        searchable-kinds (str/join "," (map kinds/code kinds/searchable-kinds))
        names (map first (apply db/query c (str "SELECT DISTINCT name FROM definition WHERE unit_id IN " q
                                                " AND kind IN (" searchable-kinds ")")
                                us))]
    ;; a unit's usage rows are found through its file elements, which hold
    ;; the whole usage key (usage has no unit_id index)
    (apply db/execute! c (str "DELETE FROM usage WHERE (to_ns, name, unit_id, name_row, name_col, lang, kind) IN (
                                 SELECT ns, name, unit_id, name_row, name_col, lang, kind FROM file_element
                                 WHERE unit_id IN " q " AND kind IN (" usage-kinds "))")
           us)
    (apply db/execute! c (str "DELETE FROM file_element WHERE unit_id IN " q) us)
    (apply db/execute! c (str "DELETE FROM doc WHERE definition_id IN (SELECT id FROM definition WHERE unit_id IN " q ")") us)
    (apply db/execute! c (str "DELETE FROM definition WHERE unit_id IN " q) us)
    (apply db/execute! c (str "DELETE FROM unit_key WHERE unit_id IN " q) us)
    (apply db/execute! c (str "DELETE FROM unit_ref WHERE unit_id IN " q) us)
    (apply db/execute! c (str "DELETE FROM unit WHERE id IN " q) us)
    (doseq [n names
            :when (nil? (db/query-value c (str "SELECT 1 FROM definition WHERE name = ? AND kind IN (" searchable-kinds ") LIMIT 1") n))]
      (db/execute! c "DELETE FROM name_fts WHERE rowid = ?" n))))

(defn- drop-dead-jars! [c]
  (let [ids (map first (db/query c "SELECT id FROM jar WHERE id NOT IN
                                    (SELECT jar_id FROM project_jar WHERE jar_id IS NOT NULL)"))]
    (doseq [j ids
            [t col] [["java_class" "jar_id"] ["jar_entry" "jar_id"] ["jar" "id"]]]
      (db/execute! c (str "DELETE FROM " t " WHERE " col " = ?") j))
    (count ids)))

(defn- drop-orphan-dep-files!
  "Opened library files whose jar is gone: their rows and extracted files."
  [c]
  (let [rows (db/query c "SELECT path FROM dep_file WHERE jar_hash NOT IN (SELECT jar_hash FROM jar)")]
    (doseq [[path] rows]
      (db/execute! c "DELETE FROM dep_file WHERE path = ?" path)
      (.delete (java.io.File. ^String path)))
    (count rows)))

(defn- sweep-symbols!
  "Symbols nothing refers to any more. An anti-join over every analysis
  table, so it only runs after something was dropped."
  [c]
  (db/execute! c "DELETE FROM sym WHERE id NOT IN (
                    SELECT ns FROM definition UNION SELECT name FROM definition
                    UNION SELECT defined_by FROM definition UNION SELECT defined_by_lint_as FROM definition
                    UNION SELECT to_ns FROM usage UNION SELECT name FROM usage
                    UNION SELECT from_ns FROM usage UNION SELECT from_var FROM usage
                    UNION SELECT ns FROM file_element UNION SELECT name FROM file_element
                    UNION SELECT alias FROM file_element
                    UNION SELECT name FROM java_class UNION SELECT ref FROM unit_ref)"))

(defn- prune-fingerprints!
  "Content-hash memos of paths no project's files or jars use (e.g. a
  deleted worktree's)."
  [c]
  (db/execute! c "DELETE FROM fingerprint WHERE path NOT IN (
                    SELECT path FROM project_file UNION SELECT path FROM project_jar)"))

(defn collect!
  "Collect garbage through writer `w`. Returns what was dropped:
  {:projects :jars :units}."
  [{:keys [c] :as w} {:keys [project-max-age-ms] :or {project-max-age-ms default-project-max-age-ms}}]
  (let [result (writer/with-write-tx w
                 (let [projects (drop-stale-projects! c project-max-age-ms)
                       jars (drop-dead-jars! c)
                       _ (drop-orphan-dep-files! c)
                       dead (map first (db/query c "SELECT id FROM unit WHERE id NOT IN (SELECT unit_id FROM project_unit)
                                                    AND id NOT IN (SELECT unit_id FROM dep_file)"))]
                   (doseq [us (partition-all batch dead)] (delete-units! c us))
                   (when (pos? (+ projects jars (count dead))) (sweep-symbols! c))
                   (prune-fingerprints! c)
                   {:projects projects :jars jars :units (count dead)}))]
    ;; the writer caches symbols and searchable names: forget deleted ones
    (writer/reload-state! w)
    (db/pragma! c "incremental_vacuum")
    result))
