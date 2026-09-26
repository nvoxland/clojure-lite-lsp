(ns csl.sources
  "Library source files, extracted from their jars into the csl home dir
  as plain read-only files, so that every editor can open them (not all
  can open jar: URIs) and so they can be analyzed and queried like any
  other file. Only the files someone navigates to are extracted.

  Layout: <home>/sources/<jar content hash, hex>/<entry path>. Keying by
  the jar's content means a new version of a library never reuses an old
  extraction."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Files StandardCopyOption]
   [java.util.jar JarFile]))

(set! *warn-on-reflection* true)

(defn- hex [^bytes bs] (apply str (map #(format "%02x" %) bs)))

(defn unhex
  "The bytes of a hex string."
  ^bytes [^String s]
  (let [bs (byte-array (quot (count s) 2))]
    (dotimes [i (alength bs)]
      (aset-byte bs i (unchecked-byte (Integer/parseInt (subs s (* 2 i) (+ 2 (* 2 i))) 16))))
    bs))

(defn sources-dir ^File [home] (io/file home "sources"))

(defn extract!
  "The path of jar location {:path jar :entry :jar-hash} extracted as a
  file, extracting it if needed."
  [home {:keys [path entry jar-hash]}]
  (let [f (io/file (sources-dir home) (hex jar-hash) ^String entry)]
    (when-not (.isFile f)
      (io/make-parents f)
      (let [tmp (io/file (.getParentFile f) (str "." (.getName f) ".tmp-" (System/nanoTime)))]
        (with-open [jf (JarFile. (str path))]
          (with-open [in (.getInputStream jf (.getJarEntry jf entry))]
            (io/copy in tmp)))
        (.setReadOnly tmp)
        (Files/move (.toPath tmp) (.toPath f) (into-array [StandardCopyOption/ATOMIC_MOVE]))))
    (str f)))

(defn source-of
  "Where an extracted file came from: {:jar-hash-hex :entry}, or nil for a
  path that isn't an extracted source."
  [home path]
  (let [base (str (sources-dir home) File/separator)]
    (when (and path (str/starts-with? path base))
      (let [[h entry] (str/split (subs path (count base)) #"/" 2)]
        (when entry {:jar-hash-hex h :entry entry})))))
