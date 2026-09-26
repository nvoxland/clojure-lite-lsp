(ns csl.fingerprint
  "Content hashes of files, memoized by (path, mtime, size) so unchanged
  files are never re-read."
  (:require
   [clojure.java.io :as io]
   [csl.db :as db])
  (:import
   [java.io File InputStream]
   [java.security MessageDigest]))

(set! *warn-on-reflection* true)

(defn sha256
  "The SHA-256 of `f`'s bytes."
  ^bytes [f]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf (byte-array 65536)]
    (with-open [^InputStream in (io/input-stream f)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.update md buf 0 n)
            (recur)))))
    (.digest md)))

(defn content-hash!
  "The content hash of the file at `path`, or nil if it doesn't exist. Uses
  and updates the fingerprint memo through writer connection `c`."
  [c path]
  (let [f (io/file path)]
    (when (.isFile ^File f)
      (let [mtime (.lastModified ^File f)
            size (.length ^File f)]
        (or (db/query-value c "SELECT content_hash FROM fingerprint WHERE path = ? AND mtime = ? AND size = ?"
                            path mtime size)
            (let [h (sha256 f)]
              (db/execute! c "INSERT INTO fingerprint (path, mtime, size, content_hash) VALUES (?, ?, ?, ?)
                              ON CONFLICT (path) DO UPDATE
                              SET mtime = excluded.mtime, size = excluded.size, content_hash = excluded.content_hash"
                           path mtime size h)
              h))))))
