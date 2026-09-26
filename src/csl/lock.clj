(ns csl.lock
  "Exclusive file locks between processes. The OS releases a lock when its
  holder dies, so \"can I take it?\" answers \"is its holder alive?\"
  exactly, unlike a pid in a file.

  Within one JVM an overlapping lock throws instead of blocking, so a lock
  held by another thread of this process counts as held too."
  (:require
   [clojure.java.io :as io])
  (:import
   [java.io File RandomAccessFile]
   [java.nio.channels FileChannel FileLock OverlappingFileLockException]))

(set! *warn-on-reflection* true)

(defn try-lock
  "Take the lock on file `path` if it's free: a handle for `release!`, or
  nil when it is held."
  [path]
  (let [f (io/file path)
        _ (io/make-parents f)
        raf (RandomAccessFile. ^File f "rw")
        ch (.getChannel raf)]
    (if-let [l (try (.tryLock ch) (catch OverlappingFileLockException _ nil))]
      {:raf raf :lock l}
      (do (.close raf) nil))))

(defn release! [{:keys [^RandomAccessFile raf ^FileLock lock]}]
  (.release lock)
  (.close raf))

(defn held?
  "Is the lock on `path` held by someone (another process or thread)?"
  [path]
  (if-let [l (try-lock path)]
    (do (release! l) false)
    true))

(defn lock!
  "Take the lock on `path`, waiting up to `timeout-ms` for it. Throws on
  timeout."
  [path timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (try-lock path)
          (if (< (System/currentTimeMillis) deadline)
            (do (Thread/sleep 20) (recur))
            (throw (ex-info (str "Timed out waiting for lock " path) {:path path})))))))
