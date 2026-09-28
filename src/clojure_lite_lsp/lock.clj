(ns clojure-lite-lsp.lock
  "Exclusive file locks between processes. The OS releases a lock when its
  holder dies, so \"can I take it?\" answers \"is its holder alive?\"
  exactly, unlike a pid in a file.

  Within one JVM an overlapping lock throws instead of blocking, so a lock
  held by another thread of this process counts as held too."
  (:require
   [clojure.java.io :as io])
  (:import
   [java.io File RandomAccessFile]
   [java.nio.channels OverlappingFileLockException]))

(set! *warn-on-reflection* true)

(defn- take-lock [path shared?]
  (let [f (io/file path)
        _ (io/make-parents f)
        raf (RandomAccessFile. ^File f "rw")]
    (try
      (if (try (.tryLock (.getChannel raf) 0 Long/MAX_VALUE (boolean shared?))
               (catch OverlappingFileLockException _ nil))
        {:raf raf}
        (do (.close raf) nil))
      (catch Throwable t (.close raf) (throw t)))))

(defn try-lock
  "Take the lock on file `path` if it's free: a handle for `release!`, or
  nil when it is held."
  [path]
  (take-lock path false))

(defn try-share
  "Take a shared lock on file `path`: any number can be held, but not
  alongside an exclusive one. A handle for `release!`, or nil."
  [path]
  (take-lock path true))

(defn release!
  "Let go of a lock taken with `try-lock`, `try-share` or `lock!`."
  [{:keys [^RandomAccessFile raf]}]
  ;; closing the file releases its lock
  (.close raf))

(defn held?
  "Is the lock on `path` held exclusively by someone (another process or
  thread)? Probes with a shared lock, so that two probes at once don't
  each see the other as a holder."
  [path]
  (if-let [l (try-share path)]
    (do (release! l) false)
    true))

(defn in-use?
  "Is any lock, shared or not, held on `path`?"
  [path]
  (if-let [l (try-lock path)]
    (do (release! l) false)
    true))

(defn poll
  "Call `f` every `interval-ms` until it returns something truthy, or
  `timeout-ms` passes: what it returned, or nil."
  ([f timeout-ms] (poll f timeout-ms 20))
  ([f timeout-ms interval-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (or (f)
           (when (< (System/currentTimeMillis) deadline)
             (Thread/sleep (long interval-ms))
             (recur)))))))

(defn lock!
  "Take the lock on `path`, waiting up to `timeout-ms` for it. Throws on
  timeout."
  [path timeout-ms]
  (or (poll #(try-lock path) timeout-ms)
      (throw (ex-info (str "Timed out waiting for lock " path) {:path path}))))
