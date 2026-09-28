(ns clojure-lite-lsp.analyze
  "Run clj-kondo and turn its results into keyed units.

  clj-kondo's own :parallel only splits work across :lint entries, so a
  list of files runs on one thread (Phase 0.2). Instead each call is split
  into shards analyzed by concurrent clj-kondo runs. They always share one
  config: hooks run in a single process-wide SCI context, so runs with
  different configs must never overlap."
  (:require
   [clj-kondo.core :as kondo]
   [clj-kondo.impl.version :as kondo-version]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.kondo-hooks :as kondo-hooks]
   [clojure-lite-lsp.normalize :as normalize]
   [clojure-lite-lsp.ns-analysis :as nsa]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io IOException]
   [java.util Arrays]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(def kondo-version kondo-version/version)

(def default-shards
  "How many clj-kondo runs share a batch."
  8)

(def ^:private modes
  {:project {:external? false :skip-lint false :analysis normalize/project-analysis-options}
   :dependency {:external? true :skip-lint true :analysis normalize/dependency-analysis-options}
   ;; a library file someone opened: everything, as for project files
   :dep-file {:external? true :skip-lint true :analysis normalize/project-analysis-options}})

(def ^:private options-hashes
  "Per mode, a hash of everything besides content and config that shapes
  its analysis."
  (update-vals modes (fn [m] (digest/sha256 (pr-str [normalize/version (dissoc m :external?)])))))

(defn- run-kondo [lint mode config-dir]
  (let [{:keys [skip-lint analysis]} (modes mode)]
    (kondo/run! {:lint lint
                 ;; the config is the project's alone: none from ~/.config
                 :repro true
                 :parallel true
                 :cache false
                 :skip-lint skip-lint
                 :config-dir config-dir
                 :config (cond-> {:auto-load-configs false
                                  :output {:canonical-paths true}
                                  :analysis analysis}
                           (not skip-lint) (assoc :linters normalize/kept-linters))})))

(defn- shard [xs n]
  (let [n (max 1 (min n (count xs)))]
    (partition-all (long (Math/ceil (/ (count xs) (double n)))) xs)))

(defn- in-shards
  "Run (f shard) for each of `n` shards of `xs` concurrently; concat the
  results in order. `f` must return realized results: its work is only
  concurrent inside the future."
  [xs n f]
  (let [runs (mapv #(future (f %)) (shard xs n))]
    ;; every run ends before a failure is thrown: analyses must never
    ;; overlap, and the next one may use another config
    (run! #(try @% (catch Throwable _)) runs)
    (into [] (mapcat deref) runs)))

(defn- canonical ^String [path] (.getCanonicalPath (io/file path)))

(defn unit-key
  "The key of the unit for a file with `content-hash` at `path` (its
  extension matters), analyzed in `mode` with `config`."
  [mode config content-hash path]
  {:content-hash content-hash
   :lang-key (normalize/file-extension path)
   :kondo-version kondo-version
   :config-hash (:hash config)
   :options-hash (options-hashes mode)
   :external? (:external? (modes mode))})

(defn- run-pass
  "One clj-kondo run over `lint` (canonical paths, or jars), with hooks'
  questions answered by `serve`: {filename {:elements :lookups}}."
  [lint mode config-dir serve]
  (let [record (atom #{})
        result (binding [kondo-hooks/*lookups* {:serve serve :record record}]
                 (run-kondo (vec lint) mode config-dir))
        units (normalize/normalize result {:external? (:external? (modes mode))})
        lookups (group-by first @record)]
    (into {} (for [f (into (set (keys units)) (keys lookups))]
               [f {:elements (get units f [])
                   :lookups (into #{} (map (fn [[_ l n d]] [l n d])) (lookups f))}]))))

(defn- batch-answers
  "{[lang ns-sym] answer} for the namespaces `results` define."
  [results]
  (let [defs (for [[filename {:keys [elements]}] results
                   e elements
                   :when (= :var-def (:kind e))]
               (assoc e :ext (normalize/file-extension filename)))]
    (into {} (for [[ns-name ds] (group-by :ns defs)
                   lang (keys nsa/lang->ext)
                   :let [a (nsa/answer lang ds)]
                   :when a]
               [[lang (symbol ns-name)] a]))))

(def ^:private max-rounds
  "How often files are analyzed again for answers the batch changed: one
  round per namespace re-exported from a re-export."
  3)

(defn- settle
  "`results` of a batch, with the files whose hooks asked about something
  the batch itself defines analyzed again (by `rerun`, given the files and
  a serve fn) until the answers they got hold."
  [results serve rerun]
  (loop [results results round 0]
    (let [serve' (let [own (batch-answers results)]
                   (fn [lang ns-sym] (or (own [lang ns-sym]) (when serve (serve lang ns-sym)))))
          redo (set (for [[filename {:keys [lookups]}] results
                          [lang ns-sym d] lookups
                          :when (not= d (nsa/digest (kondo-hooks/answer serve' lang ns-sym)))]
                      filename))]
      (if (or (empty? redo) (= max-rounds round))
        results
        (recur (merge results (rerun redo serve')) (inc round))))))

(defn- dep-refs [lookups]
  (sort (map (fn [[lang ns-sym d]] (nsa/dep-ref lang ns-sym d)) lookups)))

(defn analyze-files
  "Analyze source files with `config` ({:dir :hash} from
  clojure-lite-lsp.kondo-config) in `mode` (:project; :dependency for
  external dirs; :dep-file for a library file someone opened). Hooks asking
  about a namespace are answered by `serve` (fn [lang ns-sym], the index
  as the project sees it) or the batch itself. Returns, in order,
  [{:path :unit-key :elements}], including files with no analysis. A file
  that changed (or went) while it was analyzed has no :unit-key: what was
  analyzed isn't what a key would say."
  [paths {:keys [config mode shards serve] :or {shards default-shards}}]
  (kondo-hooks/install!)
  (kondo-hooks/use-config! (:dir config))
  (let [dir (:dir config)
        files (vec (distinct (map canonical paths)))
        content-hash #(try (digest/sha256 (io/file %)) (catch IOException _ nil))
        before (into {} (map (juxt identity content-hash)) files)
        results (into {} (in-shards files shards #(run-pass % mode dir serve)))
        results (settle results serve (fn [redo serve'] (run-pass redo mode dir serve')))]
    (vec (for [p paths
               :let [c (canonical p)
                     {:keys [elements lookups]} (results c)
                     refs (dep-refs lookups)
                     h (before c)]]
           {:path p
            :unit-key (when (and h (Arrays/equals ^bytes h ^bytes (content-hash c)))
                        (cond-> (unit-key mode config h p)
                          (seq refs) (assoc :ns-deps (vec refs))))
            :elements (into (or elements [])
                            (map (fn [r] {:kind :ref :name r :lang #{}}))
                            (when (seq refs) (cons nsa/marker refs)))}))))

(defn jar-key
  "The key of a jar with `jar-hash` analyzed with `config`."
  [jar-hash config]
  {:jar-hash jar-hash
   :config-hash (:hash config)
   :kondo-version kondo-version
   :options-hash (options-hashes :dependency)})

(defn- entry-hashes
  "{entry-path sha256} for the given entries of `jar`."
  [jar entry-paths]
  (with-open [jf (JarFile. (str jar))]
    (into {} (for [e entry-paths
                   :let [^JarEntry je (.getJarEntry jf e)]
                   :when je]
               [e (with-open [in (.getInputStream jf je)]
                    (digest/sha256 in))]))))

(defn- split-jar-path
  "clj-kondo's name for a file in a jar, \"<jar>:<entry>\", as [jar entry]."
  [filename]
  (str/split filename #"(?<=\.jar):" 2))

(defn- jar-of [filename] (first (split-jar-path filename)))
(defn- entry-of [filename] (second (split-jar-path filename)))

(defn- analyze-jar-group [jars config shards jar-hashes]
  (kondo-hooks/use-config! (:dir config))
  (in-shards jars shards
             (fn [part]
               (let [dir (:dir config)
                     by-jar (group-by (comp jar-of key) (run-pass (mapv canonical part) :dependency dir nil))]
                 ;; realized here, in the shard's future
                 (vec
                  (for [j part
                        :let [cj (canonical j)
                             ;; a jar's hooks are answered from the jar
                             ;; alone: its analysis is shared by every
                             ;; project using it
                              results (settle (into {} (by-jar cj)) nil
                                              (fn [_ serve'] (run-pass [cj] :dependency dir serve')))
                              hashes (entry-hashes j (map (comp entry-of key) results))]]
                    {:jar j
                     :jar-key (jar-key (jar-hashes j) config)
                     :entries (vec (for [[filename {:keys [elements lookups]}] (sort-by key results)
                                         :let [entry (entry-of filename)
                                               refs (dep-refs lookups)]
                                        ;; an entry that can't be read back isn't stored
                                         :when (hashes entry)]
                                     {:entry-path entry
                                      :unit-key (cond-> (unit-key :dependency config (hashes entry) entry)
                                                  (seq refs) (assoc :ns-deps (vec refs)))
                                      :elements elements}))}))))))

(defn analyze-jars
  "Analyze jars, each with its config from `configs` ({jar {:dir :hash}}).
  Jars sharing a config are sharded across concurrent runs; different
  configs run one after the other. `jar-hashes` is {jar content-hash}.
  Returns, in order, [{:jar :jar-key :entries [{:entry-path :unit-key
  :elements}]}]."
  [jars {:keys [configs jar-hashes shards] :or {shards default-shards}}]
  (kondo-hooks/install!)
  (let [;; config hashes are byte arrays: vec for value equality
        by-config (group-by (comp vec :hash configs) jars)
        results (into {}
                      (comp (mapcat (fn [[_ group]] (analyze-jar-group group (configs (first group)) shards jar-hashes)))
                            (map (juxt :jar identity)))
                      by-config)]
    (mapv results jars)))
