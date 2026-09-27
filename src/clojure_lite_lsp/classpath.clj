(ns clojure-lite-lsp.classpath
  "A project's classpath, from its build tool, and what each entry is:
  a source dir of the project (precedence 0), an external dir or a jar
  (precedence = position on the classpath)."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.db :as db])
  (:import
   [java.io File]
   [java.lang ProcessBuilder$Redirect]
   [java.security MessageDigest]
   [java.util.concurrent TimeUnit]))

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

(defn command
  "The command that prints the classpath of the project at `root`, or nil
  when it has no build file clojure-lite-lsp knows."
  [root {:keys [aliases]}]
  (cond
    (.isFile (file root "deps.edn"))
    (let [defined (set (keys (:aliases (read-edn (file root "deps.edn")))))
          use (filter defined aliases)]
      (cond-> ["clojure" "-Spath"]
        (seq use) (conj (str "-A" (str/join use)))))

    (.isFile (file root "project.clj"))
    (if (seq aliases)
      ["lein" "with-profile" (str/join "," (map #(str "+" (name %)) aliases)) "classpath"]
      ["lein" "classpath"])

    (.isFile (file root "bb.edn"))
    ["bb" "-e" "(println (babashka.classpath/get-classpath))"]))

(defn run-command
  "Run `cmd` in `dir` and return its stdout. Stdin is closed: clojure-lite-lsp lsp's own
  stdin is the LSP connection."
  [cmd dir]
  (let [p (-> (ProcessBuilder. ^java.util.List cmd)
              (.directory (io/file dir))
              (.redirectInput ProcessBuilder$Redirect/PIPE)
              (.start))
        _ (.close (.getOutputStream p))
        out (future (slurp (.getInputStream p)))
        err (future (slurp (.getErrorStream p)))]
    (if-not (.waitFor p 5 TimeUnit/MINUTES)
      (do (.destroyForcibly p)
          (throw (ex-info (str "Timed out computing the classpath: " (str/join " " cmd)) {:cmd cmd})))
      (if (zero? (.exitValue p))
        @out
        (throw (ex-info (str "Computing the classpath failed: " (str/join " " cmd) "\n" @err)
                        {:cmd cmd :exit (.exitValue p) :stderr @err}))))))

(defn- canonical [root path]
  (let [f (io/file path)
        ^File f (if (.isAbsolute f) f (io/file root path))]
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

(defn- parse [classpath-str]
  (remove str/blank? (str/split (str/trim classpath-str) (re-pattern File/pathSeparator))))

(defn compute
  "Compute the classpath of the project at `root` from scratch."
  [root & [{:keys [run] :or {run run-command}}]]
  (let [cfg (project-config root)
        cmd (command root cfg)]
    (classify root (when cmd (parse (run cmd root))) cfg)))

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
          (.getCanonicalPath (let [d (io/file dir)] (if (.isAbsolute d) d (io/file root dir)))))))))

(defn- spec-hash
  "A hash of everything the classpath is computed from: the project's
  build files, its :local/root deps' (transitively), and the user's."
  ^bytes [root cmd]
  (let [md (MessageDigest/getInstance "SHA-256")
        add! (fn [label ^File f]
               (.update md (.getBytes (str label "\u0000") "UTF-8"))
               (when (.isFile f) (.update md (.getBytes (slurp f) "UTF-8"))))]
    (.update md (.getBytes (pr-str cmd) "UTF-8"))
    (loop [todo [(.getCanonicalPath (io/file root))] seen #{}]
      (when-let [[dir & more] (seq todo)]
        (if (seen dir)
          (recur more seen)
          (do (doseq [name build-files] (add! (str dir "/" name) (file dir name)))
              (recur (into (vec more) (local-roots dir)) (conj seen dir))))))
    (doseq [f (user-build-files)] (add! (str f) f))
    (.digest md)))

(defn memoized!
  "The classpath of project `p` at `root`, recomputed only when a build file
  changed (computing it is the slowest start-up step). Uses the daemon's
  writer connection `c`.

  When computing fails (a build file mid-edit, the build tool offline),
  the last classpath that worked is used: its memo stays, under the old
  build files' hash, so the next change tries again."
  [c p root & [{:keys [run] :or {run run-command}}]]
  (let [cfg (project-config root)]
    (try
      (let [cmd (command root cfg)
            h (spec-hash root cmd)
            raw (or (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ? AND spec_hash = ?" p h)
                    (let [raw (if cmd (run cmd root) "")]
                      (db/with-tx c
                        (db/execute! c "DELETE FROM classpath_memo WHERE project_id = ?" p)
                        (db/execute! c "INSERT INTO classpath_memo (project_id, spec_hash, classpath) VALUES (?, ?, ?)"
                                     p h raw))
                      raw))]
        (classify root (parse raw) cfg))
      (catch Exception e
        (if-let [last-good (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ?" p)]
          (do (binding [*out* *err*]
                (println "clojure-lite-lsp: classpath of" root "failed, using the last one:" (ex-message e)))
              (classify root (parse last-good) cfg))
          (throw e))))))
