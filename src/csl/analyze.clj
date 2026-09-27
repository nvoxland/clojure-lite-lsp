(ns csl.analyze
  "Run clj-kondo and turn its results into keyed units.

  clj-kondo's own :parallel only splits work across :lint entries, so a
  list of files runs on one thread (Phase 0.2). Instead each call is split
  into shards analyzed by concurrent clj-kondo runs. They always share one
  config: hooks run in a single process-wide SCI context, so runs with
  different configs must never overlap."
  (:require
   [clj-kondo.core :as kondo]
   [clj-kondo.impl.version :as kondo-version]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.fingerprint :as fingerprint]
   [csl.normalize :as normalize]
   [csl.ns-analysis :as nsa])
  (:import
   [java.io File]
   [java.security MessageDigest]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(def kondo-version kondo-version/version)

(def ^:private modes
  {:project {:external? false :skip-lint false :analysis normalize/project-analysis-options}
   :dependency {:external? true :skip-lint true :analysis normalize/dependency-analysis-options}
   ;; a library file someone opened: everything, as for project files
   :dep-file {:external? true :skip-lint true :analysis normalize/project-analysis-options}})

(defn- sha256 ^bytes [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(def ^:private options-hashes
  "Per mode, a hash of everything besides content and config that shapes
  its analysis."
  (update-vals modes (fn [m] (sha256 (pr-str [normalize/version (dissoc m :external?)])))))

(def ^:dynamic *transformed*
  "While analyzing: an atom collecting [file macro] pairs (canonical paths)
  where the macro's hook returned a new node, which is how clj-kondo itself
  tells a transformation from a hook that only lints."
  nil)

(def ^:dynamic *lookups*
  "While analyzing: {:serve (fn [lang ns-sym]) :record atom}. Hooks asking
  about a namespace (hooks-api/ns-analysis) are answered by :serve, and
  [file lang ns digest] is recorded for each question."
  nil)

(defn- wrap-var!
  "Replace var `v`'s value with (wrap original), keeping the original on
  the var so reloading this namespace wraps it afresh rather than twice."
  [v wrap]
  (let [original (or (::original (meta v)) @v)]
    (alter-meta! v assoc ::original original)
    (alter-var-root v (constantly (wrap original)))))

(defn- kondo-answer
  "clj-kondo's own answer: without a cache, only its built-in namespaces."
  [lang ns-sym]
  ((or (::original (meta #'clj-kondo.hooks-api/ns-analysis*)) @#'clj-kondo.hooks-api/ns-analysis*)
   lang ns-sym))

(defn answer
  "The answer to a hook asking about `ns-sym` for `lang`, given `serve`."
  [serve lang ns-sym]
  (or (when serve (serve lang ns-sym)) (kondo-answer lang ns-sym)))

(def ^:private watch-hooks
  (delay
    ;; each hook call is compared with what it was given
    (wrap-var! #'clj-kondo.impl.hooks/hook-fn
               (fn [hook-fn]
                 (fn [ctx config ns-sym var-sym & more]
                   (when-let [f (apply hook-fn ctx config ns-sym var-sym more)]
                     (fn [{:keys [node] :as m}]
                       (let [r (f m)]
                         (when (and *transformed* (:node r) (not (identical? node (:node r))))
                           (swap! *transformed* conj [(:filename ctx) (symbol (str ns-sym) (str var-sym))]))
                         r))))))
    ;; questions about namespaces are answered by csl
    (wrap-var! #'clj-kondo.hooks-api/ns-analysis*
               (fn [ns-analysis*]
                 (fn [lang ns-sym]
                   (if-let [{:keys [serve record]} *lookups*]
                     (let [r (answer serve lang ns-sym)]
                       (swap! record conj [(:filename clj-kondo.impl.utils/*ctx*) lang ns-sym (nsa/digest r)])
                       r)
                     (ns-analysis* lang ns-sym)))))))

(defn- run-kondo [lint mode config-dir]
  (let [{:keys [skip-lint analysis]} (modes mode)]
    (kondo/run! {:lint lint
                 :parallel true
                 :cache false
                 :skip-lint skip-lint
                 :config-dir config-dir
                 :config (cond-> {:auto-load-configs false
                                  :output {:canonical-paths true}
                                  :analysis analysis}
                           (not skip-lint) (assoc :linters (normalize/only-unresolved-namespace-linter)))})))

(defn- shard [xs n]
  (let [n (max 1 (min n (count xs)))]
    (partition-all (long (Math/ceil (/ (count xs) (double n)))) xs)))

(defn- in-shards
  "Run (f shard) for each of `n` shards of `xs` concurrently; concat the
  results in order."
  [xs n f]
  (->> (shard xs n)
       (mapv #(future (f %)))
       (into [] (mapcat deref))))

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
  (let [transformed (atom #{})
        record (atom #{})
        units (normalize/normalize (binding [*transformed* transformed
                                             *lookups* {:serve serve :record record}]
                                     (run-kondo (vec lint) mode config-dir))
                                   {:external? (:external? (modes mode))})
        xforms (group-by first @transformed)
        lookups (group-by first @record)]
    (into {} (for [f (into (set (keys units)) (concat (keys xforms) (keys lookups)))]
               [f {:elements (into (get units f [])
                                   (map (fn [[_ s]] {:kind :ref :name (normalize/transformed-ref s) :lang #{}}))
                                   (xforms f))
                   :lookups (into #{} (map (fn [[_ l n d]] [l n d])) (lookups f))}]))))

(defn- batch-answers
  "{[lang ns-sym] answer} for the namespaces `results` define."
  [results]
  (let [defs (for [[filename {:keys [elements]}] results
                   e elements
                   :when (= :var-def (:kind e))]
               (assoc e :ext (normalize/file-extension filename)))]
    (into {} (for [[ns-name ds] (group-by :ns defs)
                   lang (keys nsa/ext-langs)
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
                          :when (not= d (nsa/digest (answer serve' lang ns-sym)))]
                      filename))]
      (if (or (empty? redo) (= max-rounds round))
        results
        (recur (merge results (rerun redo serve')) (inc round))))))

(defn- dep-refs [lookups]
  (sort (map (fn [[lang ns-sym d]] (nsa/dep-ref lang ns-sym d)) lookups)))

(defn analyze-files
  "Analyze source files with `config` ({:dir :hash} from csl.kondo-config)
  in `mode` (:project, or :dependency for external dirs). Hooks asking
  about a namespace are answered by `serve` (fn [lang ns-sym], the index
  as the project sees it) or the batch itself. Returns, in order,
  [{:path :unit-key :elements}], including files with no analysis."
  [paths {:keys [config mode shards serve] :or {shards 8}}]
  @watch-hooks
  (let [dir (:dir config)
        results (into {} (in-shards (vec (distinct (map canonical paths))) shards
                                    #(run-pass % mode dir serve)))
        results (settle results serve (fn [redo serve'] (run-pass redo mode dir serve')))]
    (vec (for [p paths
               :let [c (canonical p)
                     {:keys [elements lookups]} (results c)
                     refs (dep-refs lookups)]]
           {:path p
            :unit-key (cond-> (unit-key mode config (fingerprint/sha256 c) p)
                        (seq refs) (assoc :ns-deps (vec refs)))
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
                    (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in)))]))))

(defn- jar-of [filename] (first (str/split filename #"(?<=\.jar):" 2)))
(defn- entry-of [filename] (second (str/split filename #"(?<=\.jar):" 2)))

(defn- analyze-jar-group [jars config shards jar-hashes]
  (in-shards jars shards
             (fn [part]
               (let [dir (:dir config)
                     by-jar (group-by (comp jar-of key) (run-pass (mapv canonical part) :dependency dir nil))]
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
                                              refs (dep-refs lookups)]]
                                    {:entry-path entry
                                     :unit-key (cond-> (unit-key :dependency config (hashes entry) entry)
                                                 (seq refs) (assoc :ns-deps (vec refs)))
                                     :elements elements}))})))))

(defn analyze-jars
  "Analyze jars, each with its config from `configs` ({jar {:dir :hash}}).
  Jars sharing a config are sharded across concurrent runs; different
  configs run one after the other. `jar-hashes` is {jar content-hash}.
  Returns, in order, [{:jar :jar-key :entries [{:entry-path :unit-key
  :elements}]}]."
  [jars {:keys [configs jar-hashes shards] :or {shards 8}}]
  @watch-hooks
  (let [results (into {}
                      (comp (mapcat (fn [[_ group]]
                                      (analyze-jar-group (mapv first group) (second (first group)) shards jar-hashes)))
                            (map (juxt :jar identity)))
                      (group-by (comp vec :hash second) (map (juxt identity configs) jars)))]
    (mapv results jars)))
