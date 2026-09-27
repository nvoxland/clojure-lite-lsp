(ns csl.config-sig
  "What in a clj-kondo config can change analysis, and for which symbols.

  csl drops clj-kondo's findings, so linter settings never matter. What
  does: :lint-as, :hooks (and the hooks' code) and :config-in-call, each
  only for the macros a file calls; :config-in-ns, only for the file's own
  namespace; and a few global settings (:ns-groups, ...). A signature
  hashes each of those separately, so two configs can be compared per
  symbol: a file whose references (unit_ref) avoid every symbol that
  differs has the same analysis under both (csl.reuse)."
  (:require
   [clj-kondo.impl.core :as kondo-core]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk])
  (:import
   [java.io File PushbackReader StringReader]
   [java.security MessageDigest]))

(set! *warn-on-reflection* true)

(defn- canonical
  "A form whose printed text is the same for equal configs: maps and sets
  sorted."
  [x]
  (walk/postwalk (fn [v]
                   (cond
                     (map? v) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) v)
                     (set? v) (vec (sort-by pr-str v))
                     :else v))
                 x))

(defn- digest [x]
  (let [^String text (binding [*print-length* nil *print-level* nil] (pr-str (canonical x)))
        bs (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn- without-linters
  "A config section's analysis settings: nil when only linters are left."
  [m]
  (when (map? m) (not-empty (dissoc m :linters :output))))

;;;; hook code

(defn- files-under [dir]
  (let [base (count (str dir File/separator))]
    (into {}
          (keep (fn [^File f]
                  (when (.isFile f)
                    [(str/replace (subs (str f) base) File/separator "/") f])))
          (file-seq (io/file dir)))))

(defn- ns-file
  "The source file of hook namespace `ns-sym` in the config dir."
  [files ns-sym]
  (let [path (str/replace (munge (str ns-sym)) "." "/")]
    (some (fn [ext]
            (some (fn [[rel f]] (when (or (= rel (str path ext)) (str/ends-with? rel (str "/" path ext))) f))
                  files))
          [".clj" ".cljc" ".bb" ".clj_kondo"])))

(defn- required-namespaces
  "The namespaces a source file's ns form requires."
  [^File f]
  (try
    (let [form (binding [*read-eval* false]
                 (read {:eof nil :read-cond :allow} (PushbackReader. (StringReader. (slurp f)))))]
      (when (and (seq? form) (= 'ns (first form)))
        (for [clause (rest form)
              :when (and (seq? clause) (= :require (first clause)))
              spec (rest clause)
              :let [lib (if (sequential? spec) (first spec) spec)]
              :when (symbol? lib)]
          lib)))
    (catch Exception _ nil)))

(defn- hook-code
  "The code of hook namespace `ns-sym` and the hook namespaces it requires,
  from the config dir."
  [files ns-sym]
  (loop [todo [ns-sym] seen #{} code []]
    (if-let [[n & more] (seq todo)]
      (if (seen n)
        (recur more seen code)
        (if-let [f (ns-file files n)]
          (recur (into (vec more) (required-namespaces f)) (conj seen n) (conj code [n (slurp f)]))
          (recur more (conj seen n) code)))
      (sort-by (comp str first) code))))

;;;; signature

(defn signature
  "The analysis-relevant signature of the clj-kondo config dir `dir`:
  {:global hash, :entries {\"ns/name\" hash, \"ns:name\" hash}}."
  [dir]
  (let [cfg (kondo-core/resolve-config (io/file dir) [] true false)
        files (files-under dir)
        hook (fn [h] (cond
                       (symbol? h) [h (when-let [n (namespace h)] (hook-code files (symbol n)))]
                       :else h))
        {:keys [lint-as hooks config-in-call config-in-ns ns-groups]} cfg
        {:keys [analyze-call macroexpand]} hooks
        groups (set (keep :name ns-groups))
        syms (into #{} (concat (keys lint-as) (keys analyze-call) (keys macroexpand) (keys config-in-call)))
        sym-entries (into {}
                          (keep (fn [s]
                                  (let [entry [(get lint-as s)
                                               (some-> (get analyze-call s) hook)
                                               (some-> (get macroexpand s) hook)
                                               (without-linters (get config-in-call s))]]
                                    (when (some some? entry) [(str s) (digest entry)]))))
                          syms)
        [group-cfgs ns-cfgs] ((juxt filter remove) (fn [[k]] (groups k)) config-in-ns)
        ns-entries (into {}
                         (keep (fn [[n c]] (when-let [c (without-linters c)] [(str "ns:" n) (digest c)])))
                         ns-cfgs)]
    {:global (digest (-> cfg
                         (dissoc :linters :lint-as :hooks :config-in-call :config-in-ns :output :analysis
                                 ;; where things are, not what they say (hook code is
                                 ;; hashed by content, per symbol)
                                 :cfg-dir :classpath :use-import-dir :config-paths :auto-load-configs
                                 :skip-lint)
                         (assoc :other-hooks (dissoc hooks :analyze-call :macroexpand)
                                ;; config for a group of namespaces applies to whichever
                                ;; namespaces match it: global
                                :group-config (into {} (keep (fn [[g c]] (when-let [c (without-linters c)] [g c]))) group-cfgs))
                         (update :config-in-comment without-linters)))
     :entries (merge sym-entries ns-entries)}))

(defn diff
  "How two signatures differ: {:global-same? bool, :changed #{entry keys}}."
  [a b]
  {:global-same? (= (:global a) (:global b))
   :changed (into #{}
                  (filter #(not= (get (:entries a) %) (get (:entries b) %)))
                  (concat (keys (:entries a)) (keys (:entries b))))})
