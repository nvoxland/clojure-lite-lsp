(ns clojure-lite-lsp.kondo-config
  "The clj-kondo config dirs clojure-lite-lsp analyzes with. clj-kondo never gets a
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
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.fingerprint :as fingerprint]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file FileAlreadyExistsException Files StandardCopyOption]
   [java.util Properties]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(defn- jar-entries
  "[[name bytes] ...] for the jar's entries whose name matches `pred`."
  [jar pred]
  (with-open [jf (JarFile. (str jar))]
    (->> (enumeration-seq (.entries jf))
         (filter (fn [^JarEntry e] (and (not (.isDirectory e)) (pred (.getName e)))))
         (mapv (fn [^JarEntry e]
                 [(.getName e) (with-open [in (.getInputStream jf e)] (.readAllBytes in))])))))

(defn- relative-path
  "`f`'s path under directory `dir`, with / separators."
  [^File dir ^File f]
  (str/replace (str (.relativize (.toPath dir) (.toPath f))) File/separator "/"))

(defn- files-under
  "[[relative-path file] ...] of the files under directory `dir`."
  [dir]
  (let [d (io/file dir)]
    (for [^File f (file-seq d) :when (.isFile f)]
      [(relative-path d f) f])))

(defn- export-path
  "The part of `path` after a clj-kondo.exports segment, or nil. Like
  clj-kondo, the segment can be anywhere: malli ships its config at
  resources/clj-kondo/clj-kondo.exports/metosin/malli/."
  [path]
  (second (re-find #"(?:^|/)clj-kondo\.exports/(.+)$" path)))

(def ^:private jar-exports
  (fingerprint/memoize-by-file
   (fn [jar]
     (into {}
           (map (fn [[n bs]] [(export-path n) bs]))
           (jar-entries jar #(some? (export-path %)))))))

(defn exports
  "The clj-kondo configs a jar or directory exports, as {path bytes} with
  paths relative to their `clj-kondo.exports/`."
  [path]
  (if (.isDirectory (io/file path))
    (into {}
          (keep (fn [[rel ^File f]]
                  (when-let [p (export-path rel)]
                    [p (Files/readAllBytes (.toPath f))])))
          (files-under path))
    (jar-exports path)))

(defn- tag [xml t] (some-> (re-find (re-pattern (str "<" t ">\\s*([^<]*?)\\s*</" t ">")) xml) second))

(def ^:private pom-info
  "{:coords [group artifact] :deps #{[group artifact]}} of the jar at a
  path, from its Maven pom (nil when it has none, or several as in an
  uberjar); test dependencies left out."
  (fingerprint/memoize-by-file
   (fn [jar]
     (let [poms (jar-entries jar #(re-matches #"META-INF/maven/[^/]+/[^/]+/pom\.(properties|xml)" %))
           only (fn [suffix] (let [[[_ bs] :as found] (filter #(str/ends-with? (first %) suffix) poms)]
                               (when (= 1 (count found)) bs)))
           coords (when-let [bs (only "pom.properties")]
                    (let [p (doto (Properties.) (.load (io/input-stream ^bytes bs)))]
                      [(.getProperty p "groupId") (.getProperty p "artifactId")]))
           deps (when-let [bs (only "pom.xml")]
                  (->> (re-seq #"(?s)<dependency>(.*?)</dependency>" (String. ^bytes bs "UTF-8"))
                       (map second)
                       (remove #(= "test" (tag % "scope")))
                       (map (fn [d] [(str/replace (or (tag d "groupId") "") "${project.groupId}" (str (first coords)))
                                     (tag d "artifactId")]))
                       set))]
       {:coords coords :deps deps}))))

(defn maven-coords
  "The [group artifact] a jar was published as, or nil (none, or several as
  in an uberjar)."
  [jar]
  (:coords (pom-info jar)))

(defn maven-deps
  "The [group artifact]s a jar's pom declares, except test dependencies."
  [jar]
  (:deps (pom-info jar)))

(defn- reachable
  "What `start` reaches through `edges` (a fn of a node to its next
  nodes), not counting itself."
  [edges start]
  (loop [seen #{} frontier (edges start)]
    (if (empty? frontier)
      (disj seen start)
      (let [seen (into seen frontier)]
        (recur seen (into #{} (comp (mapcat edges) (remove seen)) frontier))))))

(defn dependency-closure
  "{jar #{jars it depends on, transitively}}, among `jars`."
  [jars]
  (let [by-coords (into {} (keep (fn [j] (some-> (maven-coords j) (vector j)))) jars)
        direct (zipmap jars (map #(into #{} (keep by-coords) (maven-deps %)) jars))]
    (zipmap jars (map #(reachable direct %) jars))))

(defn- content-hash ^bytes [files]
  (apply digest/sha256 (for [[path ^bytes bs] (sort-by key files)
                             part [(str path "\u0000" (count bs) "\u0000") bs]]
                         part)))

(defn materialize!
  "Write `files` ({relative-path bytes}) as a config dir under
  `cache-dir`/configs, named by the hash of its content, unless it exists.
  Returns {:dir :hash}."
  [cache-dir files]
  (let [h (content-hash files)
        dir (io/file cache-dir "configs" (digest/hex h))]
    (when-not (.isDirectory dir)
      (let [tmp (io/file cache-dir "configs" (str (digest/hex h) ".tmp-" (System/nanoTime)))
            inside (str (.getCanonicalPath tmp) File/separator)]
        (try
          (.mkdirs tmp)
          (doseq [[path ^bytes bs] files]
            (let [f (io/file tmp ^String path)]
              ;; paths come from jars: none may leave the config dir (zip-slip)
              (when-not (str/starts-with? (.getCanonicalPath f) inside)
                (throw (ex-info (str "Config file outside its dir: " path) {:path path})))
              (io/make-parents f)
              (io/copy bs f)))
          ;; atomic, so a crash never leaves a half-written config under its hash
          (Files/move (.toPath tmp) (.toPath dir) (into-array [StandardCopyOption/ATOMIC_MOVE]))
          ;; another process wrote the same config meanwhile: that one will do
          (catch FileAlreadyExistsException _ nil)
          (finally
            (when (.exists tmp)
              (run! #(.delete ^File %) (reverse (file-seq tmp))))))))
    {:dir (str dir) :hash h}))

(defn- lib-dir
  "The <org>/<lib> an export's path starts with."
  [path]
  (str/join "/" (take 2 (str/split path #"/"))))

(defn- own-config-files
  "The project's own `.clj-kondo` config as {path bytes}. Left out, so that
  a main checkout and a fresh worktree of the same branch get the same
  config (else every file of the worktree is analyzed again):
  - clj-kondo's caches and the files it generates;
  - configs copied from dependencies (copy-configs puts them at
    <org>/<lib>/, or imports/<org>/<lib>/): `exported` are the <org>/<lib>
    dirs of the classpath's exports, which are imported afresh anyway;
  - bookkeeping files at the top (.lock, .deps.edn.md5sum, ...)."
  [root exported]
  (let [d (io/file root ".clj-kondo")]
    (into {}
          (comp (remove (fn [[path]]
                          (or (re-find #"^(\.cache|inline-configs|gen-macros|imports)/" path)
                              (re-find #"^\.[^/]*$" path)
                              (exported (lib-dir path)))))
                (map (fn [[path ^File f]] [path (Files/readAllBytes (.toPath f))])))
          (when (.isDirectory d) (files-under d)))))

(defn- imports [exports-maps]
  (into {} (for [m exports-maps, [path bs] m] [(str "imports/" path) bs])))

(defn project-config!
  "The config dir for project sources at `root`, whose classpath is
  `entries` (from clojure-lite-lsp.classpath). Returns {:dir :hash}."
  [cache-dir root entries]
  (let [exports-maps (map (comp exports :path) entries)
        exported (into #{} (comp (mapcat keys) (map lib-dir)) exports-maps)]
    (materialize! cache-dir (merge (own-config-files root exported)
                                   (imports exports-maps)))))

(defn jar-context
  "What `jar-config!` needs to know about a classpath's jars."
  [jars]
  {:closure (dependency-closure jars)
   :exports (zipmap jars (map exports jars))})

(defn jar-config!
  "The config dir for analyzing `jar`: its own and its dependencies'
  exports. Returns {:dir :hash}."
  [cache-dir {:keys [closure] :as ctx} jar]
  (materialize! cache-dir (imports (map (:exports ctx) (cons jar (sort (closure jar)))))))

(defn dir-config!
  "The config dir for an external source dir (e.g. a git dep): its own
  exports only."
  [cache-dir dir]
  (materialize! cache-dir (imports [(exports dir)])))

(defn neutral-config!
  "The config dir with no configuration: clj-kondo's defaults."
  [cache-dir]
  (materialize! cache-dir {}))
