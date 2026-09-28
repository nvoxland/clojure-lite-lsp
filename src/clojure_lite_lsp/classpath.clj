(ns clojure-lite-lsp.classpath
  "A project's classpath, from its build tool, and what each entry is:
  a source dir of the project (precedence 0), an external dir or a jar
  (precedence = position on the classpath)."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.log :as log]
   [clojure-lite-lsp.process :as process]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Files]
   [java.util.regex Pattern]))

(set! *warn-on-reflection* true)

(def default-config {:aliases [:dev :test] :extra-source-paths []})

(def build-files
  "Files whose content determines the classpath."
  ["deps.edn" "project.clj" "bb.edn" ".clojure-lite-lsp.edn"])

(defn- file ^File [root name] (io/file root ^String name))

(defn- read-edn [f]
  ;; tolerate tagged literals in build files
  (edn/read-string {:default (fn [_tag v] v)} (slurp f)))

(defn project-config
  "The project's `.clojure-lite-lsp.edn`, over the defaults."
  [root]
  (let [f (file root ".clojure-lite-lsp.edn")]
    (merge default-config (when (.isFile f) (read-edn f)))))

(defn commands
  "The commands whose printed classpaths, joined, are the classpath of the
  project at `root`: its build tool's (deps.edn, else project.clj), and
  babashka's when it has a bb.edn too. Empty when it has no build file
  clojure-lite-lsp knows."
  [root {:keys [aliases]}]
  (let [deps-edn (file root "deps.edn")
        build-tool (cond
                     (.isFile deps-edn)
                     (let [defined (set (keys (:aliases (read-edn deps-edn))))
                           enabled (filter defined aliases)]
                       (cond-> ["clojure" "-Spath"]
                         (seq enabled) (conj (str "-A" (str/join enabled)))))

                     (.isFile (file root "project.clj"))
                     (if (seq aliases)
                       ["lein" "with-profile" (str/join "," (map #(str "+" (name %)) aliases)) "classpath"]
                       ["lein" "classpath"]))]
    (cond-> []
      build-tool (conj build-tool)
      (.isFile (file root "bb.edn")) (conj ["bb" "-e" "(println (babashka.classpath/get-classpath))"]))))

(defn run-command
  "Run `cmd` in `dir`: what it prints. Throws when it fails."
  [cmd dir]
  (let [{:keys [exit out err]} (process/run cmd {:dir dir :timeout-ms (* 5 60 1000)})]
    (if (zero? exit)
      out
      (throw (ex-info (str "Computing the classpath failed: " (str/join " " cmd) "\n" err)
                      {:cmd cmd :exit exit :stderr err})))))

(defn resolve-file
  "`path` as a file, resolved against `root` when relative."
  ^File [root path]
  (let [f (io/file path)]
    (if (.isAbsolute f) f (io/file root path))))

(defn- canonical [root path]
  (let [f (resolve-file root path)]
    (when (.exists f) (.getCanonicalPath f))))

(defn classify
  "Classpath `entries` (strings, relative to `root` or absolute) as
  [{:path :kind :ord}], without missing entries or duplicates. Kinds:
  :source-dir (under root, ord 0), :external-dir, :jar (ord = position among
  the remaining entries). `extra-source-paths` are added as source dirs."
  [root entries {:keys [extra-source-paths]}]
  (let [root (.getCanonicalPath (io/file root))
        under-root? #(or (= root %) (str/starts-with? % (str root File/separator)))
        paths (->> entries (keep #(canonical root %)) distinct vec)
        classified (map-indexed
                    (fn [i path]
                      (cond
                        (str/ends-with? path ".jar") {:path path :kind :jar :ord (inc i)}
                        (under-root? path) {:path path :kind :source-dir :ord 0}
                        :else {:path path :kind :external-dir :ord (inc i)}))
                    paths)
        extra (->> extra-source-paths
                   (keep #(canonical root %))
                   (remove (set paths))
                   (map (fn [path] {:path path :kind :source-dir :ord 0})))]
    (vec (concat classified extra))))

(defn parse
  "The entries of a classpath string."
  [classpath-str]
  (remove str/blank? (str/split (str/trim classpath-str) (re-pattern (Pattern/quote File/pathSeparator)))))

(defn- fallback-paths
  "Source dirs to go on with when there's no classpath: deps.edn's :paths
  if it can be read, else src and test."
  [root]
  (let [paths (try (let [f (file root "deps.edn")] (when (.isFile f) (:paths (read-edn f))))
                   (catch Exception _ nil))]
    (filter #(.isDirectory (io/file root %)) (or (seq (filter string? paths)) ["src" "test"]))))

(defn- run-all
  "The classpath the `cmds` print, joined; with none (no build file), the
  folder's src and test."
  [root cmds run]
  (if (seq cmds)
    (str/join File/pathSeparator (map #(str/trim (run % root)) cmds))
    (str/join File/pathSeparator (fallback-paths root))))

(defn compute
  "Compute the classpath of the project at `root` from scratch. `run` runs
  a command (`run-command`)."
  ([root] (compute root {}))
  ([root {:keys [run] :or {run run-command}}]
   (let [cfg (project-config root)]
     (classify root (parse (run-all root (commands root cfg) run)) cfg))))

(defn- user-build-files
  "Build files outside the project that shape its classpath."
  []
  [(io/file (or (System/getenv "CLJ_CONFIG") (io/file (System/getProperty "user.home") ".clojure")) "deps.edn")
   (io/file (System/getProperty "user.home") ".lein" "profiles.clj")])

(defn- local-roots
  "The :local/root dirs of the deps.edn at `root` (its :deps and its
  aliases' deps), absolute."
  [root]
  (let [f (file root "deps.edn")]
    (when (.isFile f)
      (let [edn (try (read-edn f) (catch Exception _ nil))
            dep-maps (cons (:deps edn)
                           (mapcat (juxt :extra-deps :replace-deps :override-deps) (vals (:aliases edn))))]
        (for [deps dep-maps
              [_ coord] deps
              :let [dir (:local/root coord)]
              :when (string? dir)]
          (.getCanonicalPath (resolve-file root dir)))))))

(defn- spec-hash
  "A hash of everything the classpath is computed from: the commands, the
  project's build files, its :local/root deps' (transitively), and the
  user's."
  ^bytes [root cmds]
  (let [dirs (loop [todo [(.getCanonicalPath (io/file root))] seen []]
               (if-let [[dir & more] (seq todo)]
                 (if (some #{dir} seen)
                   (recur more seen)
                   (recur (into (vec more) (local-roots dir)) (conj seen dir)))
                 seen))
        files (concat (for [dir dirs, build-file build-files] (file dir build-file))
                      (user-build-files))]
    (apply digest/sha256
           (pr-str cmds)
           (for [^File f files
                 part [(str f "\u0000") (if (.isFile f) (Files/readAllBytes (.toPath f)) (byte-array 0))]]
             part))))

(defn- error-key [p] (str "classpath_error:" p))

(defn error
  "Why project `p`'s classpath couldn't be computed, when it couldn't."
  [c p]
  (db/query-value c "SELECT value FROM meta WHERE key = ?" (error-key p)))

(defn memoized!
  "The classpath of project `p` at `root`, recomputed only when a build file
  changed (computing it is the slowest start-up step). Uses the daemon's
  writer connection `c`.

  When computing fails (a build file mid-edit, the build tool offline),
  the last classpath that worked is used: its memo stays, under the old
  build files' hash, so the next change tries again."
  ([c p root] (memoized! c p root {}))
  ([c p root {:keys [run] :or {run run-command}}]
   (let [cfg (project-config root)]
    (try
      (let [cmds (commands root cfg)
            h (spec-hash root cmds)
            raw (or (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ? AND spec_hash = ?" p h)
                    (let [raw (run-all root cmds run)]
                      (db/with-tx c
                        (db/execute! c "DELETE FROM classpath_memo WHERE project_id = ?" p)
                        (db/execute! c "INSERT INTO classpath_memo (project_id, spec_hash, classpath) VALUES (?, ?, ?)"
                                     p h raw))
                      raw))]
        (db/execute! c "DELETE FROM meta WHERE key = ?" (error-key p))
        (classify root (parse raw) cfg))
      (catch Exception e
        (log/warn "classpath of" root "failed:" (ex-message e))
        (db/execute! c "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)" (error-key p) (str (ex-message e)))
        ;; the last one that worked, else the usual source dirs
        (if-let [last-good (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ?" p)]
          (classify root (parse last-good) cfg)
          (classify root (map #(str (io/file root %)) (fallback-paths root)) cfg)))))))
