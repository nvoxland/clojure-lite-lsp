(ns clojure-lite-lsp.sources
  "Library source files, extracted from their jars into the clojure-lite-lsp home dir
  as plain read-only files, so that every editor can open them (not all
  can open jar: URIs) and so they can be analyzed and queried like any
  other file. Only the files someone navigates to are extracted.

  Layout: <home>/sources/<jar content hash, hex>/<entry path>. Keying by
  the jar's content means a new version of a library never reuses an old
  extraction."
  (:require
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.fingerprint :as fingerprint]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Files StandardCopyOption]
   [java.util.concurrent ExecutorService Executors ThreadFactory]
   [java.util.jar JarFile]))

(set! *warn-on-reflection* true)

(defn sources-dir
  "Where extracted sources live, canonical: the paths handed to editors and
  the ones recognized from them must agree even when the home dir is
  reached through a symlink (macOS's /var -> /private/var)."
  ^File [home]
  (.getCanonicalFile (io/file home "sources")))

(def file-hash
  "The content hash of a jar or zip at a path, remembered while it's
  unchanged."
  (fingerprint/memoize-by-file #(digest/sha256 (io/file %))))

(defn extracted-file
  "Where jar location {:entry :jar-hash} is (or will be) extracted, or nil
  when its entry would land outside the sources dir (zip-slip)."
  ^File [home {:keys [entry jar-hash]}]
  (let [dir (io/file (sources-dir home) (digest/hex jar-hash))
        f (.getCanonicalFile (io/file dir ^String entry))]
    (when (str/starts-with? (str f) (str dir File/separator))
      f)))

(defn extract!
  "The path of jar location {:path jar :entry :jar-hash} extracted as a
  file, extracting it if needed. nil when that can't be done faithfully:
  the entry would land outside the sources dir, the jar is gone or no
  longer has the content its hash says (a replaced snapshot: its new
  content must not be kept under the old hash), or it lacks the entry."
  [home {:keys [path entry jar-hash] :as loc}]
  (when-let [f (extracted-file home loc)]
    (if (.isFile f)
      (str f)
      (when (and (.isFile (io/file (str path)))
                 (java.util.Arrays/equals ^bytes jar-hash ^bytes (file-hash path)))
        (with-open [jf (JarFile. (str path))]
          (when-let [je (.getJarEntry jf ^String entry)]
            (io/make-parents f)
            ;; written aside, then moved into place: a reader never sees half a file
            (let [tmp (io/file (.getParentFile f) (str "." (.getName f) ".tmp-" (System/nanoTime)))]
              (try
                (with-open [in (.getInputStream jf je)]
                  (io/copy in tmp))
                (.setReadOnly tmp)
                (Files/move (.toPath tmp) (.toPath f) (into-array [StandardCopyOption/ATOMIC_MOVE]))
                (finally (io/delete-file tmp true))))
            (str f)))))))

(defonce ^:private extractor
  ;; one at a time, off the editor's request thread; made on first use
  ;; (namespaces are initialized when the native image is built, and a
  ;; thread pool can't be part of it)
  (delay (Executors/newSingleThreadExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. r "clojure-lite-lsp-extract") (.setDaemon true)))))))

(defn extract-soon!
  "The path jar location `loc` will be extracted to, extracting it in the
  background: for results listed before anyone picks one (symbol search).
  nil when it can't be extracted there."
  [home loc]
  (when-let [f (extracted-file home loc)]
    (when-not (.isFile f)
      (.submit ^ExecutorService @extractor ^Runnable (fn [] (try (extract! home loc) (catch Exception _ nil)))))
    (str f)))

(defn source-of
  "Where an extracted file came from: {:jar-hash-hex :entry}, or nil for a
  path that isn't an extracted source."
  [home path]
  (let [base (str (sources-dir home) File/separator)]
    (when (and path (str/starts-with? path base))
      (let [[h entry] (str/split (subs path (count base)) #"/" 2)]
        (when entry {:jar-hash-hex h :entry entry})))))
