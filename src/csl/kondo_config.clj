(ns csl.kondo-config
  "The clj-kondo config dirs csl analyzes with. clj-kondo never gets a
  project's own `.clj-kondo`: given a config dir, it writes and deletes
  generated files there (DESIGN.md D17). Instead every config is
  materialized into a private directory named by the hash of its content,
  which is also the unit key's config hash.

  - Project sources: the project's `.clj-kondo` (minus clj-kondo's caches and
    generated files) plus every `clj-kondo.exports` on the classpath, under
    `imports/`.
  - A jar: only the exports of the jar and of its Maven dependencies
    (transitively, among the classpath's jars). Jars with none share the
    empty neutral config (DESIGN.md §4.2).

  Runs must also pass `{:auto-load-configs false}` in clj-kondo's :config
  option: that stops it writing inline configs into these directories,
  while the config dir's own auto-loading of imports still applies."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Files StandardCopyOption]
   [java.security MessageDigest]
   [java.util Properties]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(def ^:private exports-prefix "clj-kondo.exports/")

(defn- jar-entries
  "[[name bytes] ...] for the jar's entries whose name matches `pred`."
  [jar pred]
  (with-open [jf (JarFile. (str jar))]
    (->> (enumeration-seq (.entries jf))
         (filter (fn [^JarEntry e] (and (not (.isDirectory e)) (pred (.getName e)))))
         (mapv (fn [^JarEntry e]
                 [(.getName e) (with-open [in (.getInputStream jf e)] (.readAllBytes in))])))))

(defn exports
  "The clj-kondo configs a jar or directory exports, as {path bytes} with
  paths relative to `clj-kondo.exports/`."
  [path]
  (let [f (io/file path)]
    (if (.isDirectory f)
      (let [root (io/file f exports-prefix)
            base (count (str root File/separator))]
        (into {}
              (comp (filter #(.isFile ^File %))
                    (map (fn [^File x] [(str/replace (subs (str x) base) File/separator "/")
                                        (Files/readAllBytes (.toPath x))])))
              (when (.isDirectory root) (file-seq root))))
      (into {}
            (map (fn [[n bs]] [(subs n (count exports-prefix)) bs]))
            (jar-entries f #(str/starts-with? % exports-prefix))))))

(defn- pom-files [jar]
  (jar-entries jar #(re-matches #"META-INF/maven/[^/]+/[^/]+/pom\.(properties|xml)" %)))

(defn maven-coords
  "The [group artifact] a jar was published as, or nil (none, or several as
  in an uberjar)."
  [jar]
  (let [props (filter #(str/ends-with? (first %) "pom.properties") (pom-files jar))]
    (when (= 1 (count props))
      (let [p (doto (Properties.) (.load (io/input-stream ^bytes (second (first props)))))]
        [(.getProperty p "groupId") (.getProperty p "artifactId")]))))

(defn- tag [xml t] (some-> (re-find (re-pattern (str "<" t ">\\s*([^<]*?)\\s*</" t ">")) xml) second))

(defn maven-deps
  "The [group artifact]s a jar's pom declares, except test dependencies."
  [jar]
  (let [xmls (filter #(str/ends-with? (first %) "pom.xml") (pom-files jar))]
    (when (= 1 (count xmls))
      (let [xml (String. ^bytes (second (first xmls)) "UTF-8")
            own-group (first (maven-coords jar))]
        (->> (re-seq #"(?s)<dependency>(.*?)</dependency>" xml)
             (map second)
             (remove #(= "test" (tag % "scope")))
             (map (fn [d] [(str/replace (or (tag d "groupId") "") "${project.groupId}" (str own-group))
                           (tag d "artifactId")]))
             set)))))

(defn dependency-closure
  "{jar #{jars it depends on, transitively}}, among `jars`."
  [jars]
  (let [by-coords (into {} (keep (fn [j] (when-let [c (maven-coords j)] [c j]))) jars)
        direct (into {} (map (fn [j] [j (set (keep by-coords (maven-deps j)))])) jars)
        closure (fn [j]
                  (loop [todo (vec (direct j)) seen #{}]
                    (if-let [[x & more] (seq todo)]
                      (if (or (seen x) (= x j))
                        (recur (vec more) seen)
                        (recur (into (vec more) (direct x)) (conj seen x)))
                      seen)))]
    (into {} (map (fn [j] [j (closure j)])) jars)))

(defn- hex [^bytes bs] (apply str (map #(format "%02x" %) bs)))

(defn- content-hash ^bytes [files]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (doseq [[path ^bytes bs] (sort-by key files)]
      (.update md (.getBytes (str path "\u0000" (count bs) "\u0000") "UTF-8"))
      (.update md bs))
    (.digest md)))

(defn materialize!
  "Write `files` ({relative-path bytes}) as a config dir under
  `cache-dir`/configs, named by the hash of its content, unless it exists.
  Returns {:dir :hash}."
  [cache-dir files]
  (let [h (content-hash files)
        dir (io/file cache-dir "configs" (hex h))]
    (when-not (.isDirectory dir)
      (let [tmp (io/file cache-dir "configs" (str (hex h) ".tmp-" (System/nanoTime)))]
        (.mkdirs tmp)
        (doseq [[path ^bytes bs] files]
          (let [f (io/file tmp ^String path)]
            (io/make-parents f)
            (Files/write (.toPath f) bs ^"[Ljava.nio.file.OpenOption;" (into-array java.nio.file.OpenOption []))))
        ;; atomic, so a crash never leaves a half-written config under its hash
        (Files/move (.toPath tmp) (.toPath dir) (into-array [StandardCopyOption/ATOMIC_MOVE]))))
    {:dir (str dir) :hash h}))

(defn- own-config-files
  "The project's `.clj-kondo` as {path bytes}, without clj-kondo's caches
  and the files it generates."
  [root]
  (let [d (io/file root ".clj-kondo")
        base (count (str d File/separator))]
    (into {}
          (comp (filter #(.isFile ^File %))
                (map (fn [^File f] [(str/replace (subs (str f) base) File/separator "/") f]))
                (remove (fn [[path]] (re-find #"^(\.cache|inline-configs|gen-macros)/" path)))
                (map (fn [[path ^File f]] [path (Files/readAllBytes (.toPath f))])))
          (when (.isDirectory d) (file-seq d)))))

(defn- imports [exports-maps]
  (into {} (for [m exports-maps, [path bs] m] [(str "imports/" path) bs])))

(defn project-config!
  "The config dir for project sources at `root`, whose classpath is
  `entries` (from csl.classpath). Returns {:dir :hash}."
  [cache-dir root entries]
  (materialize! cache-dir (merge (own-config-files root)
                                 (imports (map (comp exports :path) entries)))))

(defn jar-context
  "What `jar-config!` needs to know about a classpath's jars."
  [jars]
  {:closure (dependency-closure jars)
   :exports (into {} (map (fn [j] [j (exports j)])) jars)})

(defn jar-config!
  "The config dir for analyzing `jar`: its own and its dependencies'
  exports. Returns {:dir :hash}."
  [cache-dir {:keys [closure] :as ctx} jar]
  (materialize! cache-dir (imports (map (:exports ctx) (cons jar (sort (closure jar)))))))

(defn neutral-config!
  "The config dir with no configuration: clj-kondo's defaults."
  [cache-dir]
  (materialize! cache-dir {}))
