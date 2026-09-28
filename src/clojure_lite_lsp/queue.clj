(ns clojure-lite-lsp.queue
  "The work queue in the `pending` table. `clojure-lite-lsp lsp` enqueues; the daemon
  takes batches and marks them done.

  Kinds: :sync (recompute a project's classpath, config and file list;
  path \"\"), :file, :delete, :jar, :dep-file (an opened library file). Lower priority runs first:
  0 files open in an editor, 1 project work, 2 jars."
  (:require
   [clojure-lite-lsp.db :as db]))

(def default-batch-sizes
  "How many requests of a kind one batch takes (DESIGN.md §5.3)."
  {:file 100 :delete 1000 :jar 8 :sync 1 :dep-file 20})

(defn enqueue!
  "Ask for `kind` work on `path` of project `p`. Re-enqueueing only ever
  raises a request's priority, and restamps it so an in-flight batch
  doesn't drop it (see `done!`).

  `enqueued_at` is a sequence number from the database, not a clock:
  clients and the daemon are different processes. Writes are serialized,
  so it strictly increases among queued requests."
  [c p kind path priority]
  (db/with-tx c
    (db/execute! c "INSERT INTO pending (project_id, kind, path, priority, enqueued_at)
                    VALUES (?, ?, ?, ?, (SELECT coalesce(max(enqueued_at), 0) + 1 FROM pending))
                    ON CONFLICT (project_id, kind, path) DO UPDATE
                    SET priority = min(priority, excluded.priority), enqueued_at = excluded.enqueued_at"
                 p (name kind) path priority)
    ;; work on a project is seeing it: GC drops projects unseen for long
    (db/execute! c "UPDATE project SET last_seen = ? WHERE id = ?" (System/currentTimeMillis) p)))

(def ^:private select-requests
  "SELECT project_id, kind, path, priority, enqueued_at FROM pending ")

(defn- row->request [[p kind path priority enqueued-at]]
  {:project-id p :kind (keyword kind) :path path :priority priority :enqueued-at enqueued-at})

(defn request-key
  "What identifies a request in the queue."
  [{:keys [project-id kind path]}]
  [project-id kind path])

(defn next-batch
  "The next batch: requests of the top priority for one project and kind,
  up to that kind's batch size. Empty when the queue is empty. Requests
  whose keys are in `in-flight` (taken but not done yet) are skipped."
  ([c batch-sizes] (next-batch c batch-sizes nil))
  ([c batch-sizes in-flight]
   (let [skip? (set in-flight)
         ;; fetch enough to have what's wanted once the skipped are removed
         skipped (count skip?)
         requests #(->> % (map row->request) (remove (comp skip? request-key)))]
     (if-let [{:keys [project-id kind priority]}
              (first (requests (db/query c (str select-requests "ORDER BY priority, enqueued_at LIMIT ?")
                                         (inc skipped))))]
       (let [size (get (merge default-batch-sizes batch-sizes) kind 100)]
         (->> (db/query c (str select-requests "WHERE project_id = ? AND kind = ? AND priority = ?
                                                ORDER BY enqueued_at LIMIT ?")
                        project-id (name kind) priority (+ skipped size))
              requests
              (take size)
              vec))
       []))))

(defn done!
  "Remove a processed batch, except requests re-enqueued since it was taken."
  [c batch]
  (db/with-tx c
    (doseq [{:keys [project-id kind path enqueued-at]} batch]
      (db/execute! c "DELETE FROM pending WHERE project_id = ? AND kind = ? AND path = ? AND enqueued_at = ?"
                   project-id (name kind) path enqueued-at))))

(defn pending-count
  "How many requests project `p` has waiting."
  [c p]
  (db/query-value c "SELECT count(*) FROM pending WHERE project_id = ?" p))
