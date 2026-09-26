(ns csl.java
  "Where a Java class used from Clojure is written, when its source is
  available. csl is a Clojure server: it doesn't analyze Java, it only
  finds a class's source file (and its declaration line) so navigation
  from Clojure interop lands somewhere readable. It looks in:

  1. the project's own .java files
  2. a -sources.jar next to a jar containing the class
  3. the JDK's lib/src.zip

  Sources inside jars and zips are extracted like library files
  (csl.sources). A class with no source anywhere has no location:
  decompiling is out of scope."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.fingerprint :as fingerprint]
   [csl.sources :as sources])
  (:import
   [java.io File]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(defn- source-entry
  "The source path of a class: acme.Widget$Part -> acme/Widget.java."
  [class-name]
  (str (str/replace (first (str/split class-name #"\$")) "." "/") ".java"))

(defn- simple-name [class-name]
  (last (str/split (first (str/split class-name #"\$")) #"\.")))

(defn- declaration-pos
  "Where `class-name` is declared in `text`: [row col end-row end-col], or
  the start of the file."
  [text class-name]
  (let [n (simple-name class-name)
        re (re-pattern (str "\\b(?:class|interface|enum|record|@interface)\\s+(" n ")\\b"))]
    (or (first (keep-indexed (fn [i line]
                               (let [m (re-matcher re line)]
                                 (when (.find m)
                                   [(inc i) (inc (.start m 1)) (inc i) (inc (.end m 1))])))
                             (str/split-lines text)))
        [1 1 1 1])))

(def ^:private hashes (atom {}))

(defn- file-hash
  "The content hash of a jar or zip, remembered while it's unchanged."
  [path]
  (let [f (io/file path)
        k [path (.lastModified f) (.length f)]]
    (or (@hashes k)
        (let [h (fingerprint/sha256 f)] (swap! hashes assoc k h) h))))

(def ^:private zip-entries (atom {}))

(defn- entry-ending-with
  "The entry of zip `path` that is, or ends with /, `suffix` (src.zip puts
  each class under its module: java.base/java/io/File.java)."
  [path suffix]
  (let [names (or (@zip-entries path)
                  (let [ns (with-open [z (JarFile. (str path))]
                             (mapv #(.getName ^JarEntry %) (enumeration-seq (.entries z))))]
                    (swap! zip-entries assoc path ns)
                    ns))]
    (first (filter #(or (= % suffix) (str/ends-with? % (str "/" suffix))) names))))

(defn- from-zip [home zip entry-suffix]
  (when (and zip (.isFile (io/file zip)))
    (when-let [entry (entry-ending-with zip entry-suffix)]
      (sources/extract! home {:path zip :entry entry :jar-hash (file-hash zip)}))))

(defn- sources-jar [jar]
  (str/replace jar #"\.jar$" "-sources.jar"))

(defn source-location
  "The location {:path :pos} of `class-name`'s declaration, or nil.
  `source-dirs`: directories of .java files; `class-jars`: jars known to
  contain the class; `jdk-src`: the JDK's src.zip."
  [{:keys [home source-dirs class-jars jdk-src]} class-name]
  (let [entry (source-entry class-name)
        path (or (first (filter #(.isFile (io/file ^String %))
                                (map #(str (io/file % entry)) source-dirs)))
                 (first (keep #(from-zip home (sources-jar %) entry) class-jars))
                 (from-zip home jdk-src entry))]
    (when path
      {:path path :pos (declaration-pos (slurp path) class-name)})))

(defn jdk-src
  "The JDK's src.zip: from $JAVA_HOME, else this JVM's own java.home (a
  native image has none)."
  []
  (->> [(System/getenv "JAVA_HOME") (System/getProperty "java.home")]
       (remove nil?)
       (map #(io/file % "lib" "src.zip"))
       (filter #(.isFile ^File %))
       first
       (#(some-> ^File % str))))
