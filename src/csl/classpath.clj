(ns csl.classpath
  "A project's classpath, from its build tool, and what each entry is:
  a source dir of the project (precedence 0), an external dir or a jar
  (precedence = position on the classpath)."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.db :as db])
  (:import
   [java.io File]
   [java.lang ProcessBuilder$Redirect]
   [java.security MessageDigest]
   [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

(def default-config {:aliases [:dev :test] :extra-source-paths []})

(def build-files
  "Files whose content determines the classpath."
  ["deps.edn" "project.clj" "bb.edn" ".csl.edn"])

(defn- file ^File [root name] (io/file root ^String name))

(defn- read-edn [f]
  ;; tolerate tagged literals in build files
  (edn/read-string {:default (fn [_tag v] v)} (slurp f)))

(defn project-config
  "The project's `.csl.edn`, over the defaults."
  [root]
  (let [f (file root ".csl.edn")]
    (merge default-config (when (.isFile f) (read-edn f)))))

(defn command
  "The command that prints the classpath of the project at `root`, or nil
  when it has no build file csl knows."
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
  "Run `cmd` in `dir` and return its stdout. Stdin is closed: csl lsp's own
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

(defn- spec-hash
  "A hash of everything the classpath is computed from."
  ^bytes [root cmd]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.update md (.getBytes (pr-str cmd) "UTF-8"))
    (doseq [name build-files
            :let [f (file root name)]]
      (.update md (.getBytes (str name "\u0000") "UTF-8"))
      (when (.isFile f) (.update md (.getBytes (slurp f) "UTF-8"))))
    (.digest md)))

(defn memoized!
  "The classpath of project `p` at `root`, recomputed only when a build file
  changed (computing it is the slowest start-up step). Uses the daemon's
  writer connection `c`."
  [c p root & [{:keys [run] :or {run run-command}}]]
  (let [cfg (project-config root)
        cmd (command root cfg)
        h (spec-hash root cmd)
        raw (or (db/query-value c "SELECT classpath FROM classpath_memo WHERE project_id = ? AND spec_hash = ?" p h)
                (let [raw (if cmd (run cmd root) "")]
                  (db/with-tx c
                    (db/execute! c "DELETE FROM classpath_memo WHERE project_id = ?" p)
                    (db/execute! c "INSERT INTO classpath_memo (project_id, spec_hash, classpath) VALUES (?, ?, ?)"
                                 p h raw))
                  raw))]
    (classify root (parse raw) cfg)))
