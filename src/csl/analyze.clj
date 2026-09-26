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
   [csl.normalize :as normalize])
  (:import
   [java.io File]
   [java.security MessageDigest]
   [java.util.jar JarEntry JarFile]))

(set! *warn-on-reflection* true)

(def kondo-version kondo-version/version)

(def ^:private modes
  {:project {:external? false :skip-lint false :analysis normalize/project-analysis-options}
   :dependency {:external? true :skip-lint true :analysis normalize/dependency-analysis-options}})

(defn- sha256 ^bytes [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(def ^:private options-hashes
  "Per mode, a hash of everything besides content and config that shapes
  its analysis."
  (update-vals modes (fn [m] (sha256 (pr-str [normalize/version (dissoc m :external?)])))))

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

(defn analyze-files
  "Analyze source files with `config` ({:dir :hash} from csl.kondo-config)
  in `mode` (:project, or :dependency for external dirs). Returns, in
  order, [{:path :unit-key :elements}], including files with no analysis."
  [paths {:keys [config mode shards] :or {shards 8}}]
  (in-shards paths shards
             (fn [part]
               (let [by-canonical (into {} (map (fn [p] [(canonical p) p])) part)
                     units (normalize/normalize (run-kondo (vec (keys by-canonical)) mode (:dir config))
                                                {:external? (:external? (modes mode))})]
                 (for [p part
                       :let [c (canonical p)]]
                   {:path p
                    :unit-key (unit-key mode config (fingerprint/sha256 c) p)
                    :elements (get units c [])})))))

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

(defn- analyze-jar-group [jars config shards jar-hashes]
  (in-shards jars shards
             (fn [part]
               (let [by-canonical (into {} (map (fn [j] [(canonical j) j])) part)
                     units (normalize/normalize (run-kondo (vec (keys by-canonical)) :dependency (:dir config))
                                                {:external? true})
                     by-jar (group-by (fn [[filename]] (first (str/split filename #"(?<=\.jar):" 2))) units)]
                 (for [j part
                       :let [entries (for [[filename els] (by-jar (canonical j))]
                                       [(second (str/split filename #"(?<=\.jar):" 2)) els])
                             hashes (entry-hashes j (map first entries))]]
                   {:jar j
                    :jar-key (jar-key (jar-hashes j) config)
                    :entries (vec (for [[entry els] (sort-by first entries)]
                                    {:entry-path entry
                                     :unit-key (unit-key :dependency config (hashes entry) entry)
                                     :elements els}))})))))

(defn analyze-jars
  "Analyze jars, each with its config from `configs` ({jar {:dir :hash}}).
  Jars sharing a config are sharded across concurrent runs; different
  configs run one after the other. `jar-hashes` is {jar content-hash}.
  Returns, in order, [{:jar :jar-key :entries [{:entry-path :unit-key
  :elements}]}]."
  [jars {:keys [configs jar-hashes shards] :or {shards 8}}]
  (let [results (into {}
                      (comp (mapcat (fn [[_ group]]
                                      (analyze-jar-group (mapv first group) (second (first group)) shards jar-hashes)))
                            (map (juxt :jar identity)))
                      (group-by (comp vec :hash second) (map (juxt identity configs) jars)))]
    (mapv results jars)))
