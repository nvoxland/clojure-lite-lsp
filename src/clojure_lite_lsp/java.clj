(ns clojure-lite-lsp.java
  "Where a Java class used from Clojure is written, when its source is
  available. clojure-lite-lsp is a Clojure server: it doesn't analyze Java, it only
  finds a class's source file (and its declaration line) so navigation
  from Clojure interop lands somewhere readable. It looks in:

  1. the project's own .java files
  2. a -sources.jar next to a jar containing the class
  3. the JDK's lib/src.zip

  Sources inside jars and zips are extracted like library files
  (clojure-lite-lsp.sources). A class with no source anywhere has no location:
  decompiling is out of scope."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.sources :as sources])
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

(def ^:private zip-prefixes (atom {}))

(defn- top-dirs
  "The top-level directories of zip `path` (src.zip's modules): kept, not
  its tens of thousands of entry names."
  [path]
  (let [f (io/file path)
        k [(str path) (.lastModified f) (.length f)]]
    (or (@zip-prefixes k)
        (let [dirs (with-open [z (JarFile. (str path))]
                     (into (sorted-set) (keep #(second (re-find #"^([^/]+)/" (.getName ^JarEntry %))))
                           (enumeration-seq (.entries z))))]
          (swap! zip-prefixes assoc k dirs)
          dirs))))

(defn- entry-ending-with
  "The entry of zip `path` that is `suffix`, or that under a top-level
  directory (src.zip puts each class under its module:
  java.base/java/io/File.java)."
  [path suffix]
  (with-open [z (JarFile. (str path))]
    (or (when (.getEntry z suffix) suffix)
        (first (filter #(.getEntry z ^String %) (map #(str % "/" suffix) (top-dirs path)))))))

(defn- from-zip [home zip entry-suffix]
  (when (and zip (.isFile (io/file zip)))
    (when-let [entry (entry-ending-with zip entry-suffix)]
      (sources/extract! home {:path zip :entry entry :jar-hash (sources/file-hash zip)}))))

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

(defn jdk-src-in
  "The JDK's src.zip, looked for where JDKs keep it: under $JAVA_HOME
  (lib/src.zip, or src.zip in JDK 8's layout), this JVM's java.home, and on
  macOS /usr/libexec/java_home's JDK. `run` runs a command, giving its
  output (nil when it fails)."
  [{:keys [java-home-env java-home-prop run]}]
  (->> [java-home-env java-home-prop (when run (some-> (run ["/usr/libexec/java_home"]) str/trim not-empty))]
       (remove nil?)
       (mapcat #(vector (io/file % "lib" "src.zip") (io/file % "src.zip")))
       (filter #(.isFile ^File %))
       first
       (#(some-> ^File % str))))

(defn- run-quietly [cmd]
  (try
    (when (.exists (io/file (first cmd)))
      (let [p (.start (doto (ProcessBuilder. ^java.util.List cmd) (.redirectErrorStream true)))
            out (slurp (.getInputStream p))]
        (when (zero? (.waitFor p)) out)))
    (catch Exception _ nil)))

(def ^:private jdk-src-memo (delay (jdk-src-in {:java-home-env (System/getenv "JAVA_HOME")
                                                :java-home-prop (System/getProperty "java.home")
                                                :run run-quietly})))

(defn jdk-src
  "The JDK's src.zip on this machine, or nil (a native image has no
  java.home of its own)."
  []
  @jdk-src-memo)

(defn- project-source-dirs
  "Where project `p`'s .java files may be: its classpath dirs, and the
  usual places (they are often compiled separately, off the classpath)."
  [c p]
  (let [root (db/query-value c "SELECT root FROM project WHERE id = ?" p)
        memo (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ?" p)]
    (distinct (concat (->> (str/split (or memo "") (re-pattern File/pathSeparator))
                           (remove #(str/ends-with? % ".jar"))
                           (map #(if (.isAbsolute (io/file ^String %)) % (str root "/" %))))
                      (map #(str root "/" %) ["java" "src/main/java" "src/java" "src"])))))

(defn class-location
  "The location {:path :pos} of `class-name`'s source for project `p`, or
  nil: its .java files, a -sources.jar beside a jar with the class, or the
  JDK's src.zip. `home`: where extracted sources go."
  [c home p class-name]
  (source-location {:home home
                    :source-dirs (project-source-dirs c p)
                    :class-jars (q/java-class-jars c p class-name)
                    :jdk-src (jdk-src)}
                   class-name))
