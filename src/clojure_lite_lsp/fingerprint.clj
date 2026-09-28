(ns clojure-lite-lsp.fingerprint
  "Content hashes of files, memoized by (path, mtime, size) so unchanged
  files are never re-read."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure.java.io :as io])
  (:import
   [java.io File IOException]))

(set! *warn-on-reflection* true)

(defn- stamp [^File f] [(.lastModified f) (.length f)])

(defn memoize-by-file
  "`f` of a file's path, remembered per path while the file's modification
  time and size stay the same."
  [f]
  (let [memo (atom {})]
    (fn [path]
      (let [k (str path)
            now (stamp (io/file k))
            [then v] (@memo k)]
        (if (= then now)
          v
          (let [v (f path)]
            (swap! memo assoc k [now v])
            v))))))

(defn content-hash!
  "The content hash of the file at `path`, or nil if it doesn't exist or
  can't be read. Uses and updates the fingerprint memo through writer
  connection `c`."
  [c path]
  (let [f (io/file path)]
    (when (and (.isFile f) (.canRead f))
      (let [[mtime size] (stamp f)]
        (or (db/query-value c "SELECT content_hash FROM fingerprint WHERE path = ? AND mtime = ? AND size = ?"
                            path mtime size)
            ;; gone or unreadable since the check: as if missing
            (when-let [h (try (digest/sha256 f) (catch IOException _ nil))]
              (db/execute! c "INSERT INTO fingerprint (path, mtime, size, content_hash) VALUES (?, ?, ?, ?)
                              ON CONFLICT (path) DO UPDATE
                              SET mtime = excluded.mtime, size = excluded.size, content_hash = excluded.content_hash"
                           path mtime size h)
              h))))))
