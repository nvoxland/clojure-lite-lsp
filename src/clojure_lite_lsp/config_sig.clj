(ns clojure-lite-lsp.config-sig
  "What in a clj-kondo config can change analysis, and for which symbols.

  clojure-lite-lsp drops clj-kondo's findings, so linter settings never matter. What
  does: :lint-as, :hooks (and the hooks' code) and :config-in-call, each
  only for the macros a file calls; :config-in-ns, only for the file's own
  namespace; and a few global settings (:ns-groups, ...). A signature
  hashes each of those separately, so two configs can be compared per
  symbol: a file whose references (unit_ref) avoid every symbol that
  differs has the same analysis under both (clojure-lite-lsp.reuse)."
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

(defn digest
  "SHA-256 hex of `x`, the same for equal values."
  [x]
  (let [^String text (binding [*print-length* nil *print-level* nil] (pr-str (canonical x)))
        bs (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn- without-linters
  "A config section's analysis settings: nil when only linters are left.
  :unresolved-namespace counts: its findings are kept as elements."
  [m]
  (when (map? m)
    (let [kept (get-in m [:linters :unresolved-namespace])]
      (not-empty (cond-> (dissoc m :linters :output)
                   kept (assoc :unresolved-namespace kept))))))

(def ^:private any-alias
  "Reads ::alias/kw without the file's aliases (a hook's requires are all
  that's needed from it)."
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_] 'user)
    (resolveClass [_ sym] sym)
    (resolveAlias [_ sym] sym)
    (resolveVar [_ sym] sym)))

;;;; hook code

(defn- files-under [dir]
  (let [base (count (str dir File/separator))]
    (into {}
          (keep (fn [^File f]
                  (when (.isFile f)
                    [(str/replace (subs (str f) base) File/separator "/") f])))
          (file-seq (io/file dir)))))

(defn- ns-files
  "The source files of hook namespace `ns-sym` in the config dir: all of
  them, when it's in more than one place (the config's own and an
  import): which one clj-kondo loads isn't for clojure-lite-lsp to guess."
  [files ns-sym]
  (let [path (str/replace (munge (str ns-sym)) "." "/")]
    (or (some (fn [ext]
                (not-empty (vec (keep (fn [[rel f]] (when (or (= rel (str path ext)) (str/ends-with? rel (str "/" path ext))) f))
                                      (sort-by key files)))))
              [".clj" ".cljc" ".bb" ".clj_kondo"])
        [])))

(defn- libs
  "The namespaces a require/use spec names: a symbol, a vector
  [lib & opts], or a prefix list (prefix lib-or-vector ...)."
  [spec]
  (cond
    (symbol? spec) [spec]
    (and (sequential? spec) (symbol? (first spec)))
    (let [[head & more] spec]
      (if (and (seq more) (every? #(or (symbol? %) (sequential? %)) more)
               (not (keyword? (first more))))
        ;; a prefix list
        (for [m more
              lib (libs m)]
          (symbol (str head "." lib)))
        [head]))
    :else []))

(defn- required-namespaces
  "The namespaces a source file requires: in its ns form's :require and
  :use, and in top-level (require ...) and (use ...) calls."
  [^File f]
  (try
    (let [forms (binding [*read-eval* false *reader-resolver* any-alias]
                  (let [r (PushbackReader. (StringReader. (slurp f)))]
                    (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof :read-cond :allow} r))))))
          unquote-spec #(if (and (seq? %) (= 'quote (first %))) (second %) %)]
      (distinct
       (concat
        (for [form forms
              :when (and (seq? form) (= 'ns (first form)))
              clause (rest form)
              :when (and (seq? clause) (#{:require :use} (first clause)))
              spec (rest clause)
              lib (libs spec)]
          lib)
        (for [form forms
              :when (and (seq? form) ('#{require use} (first form)))
              spec (rest form)
              lib (libs (unquote-spec spec))]
          lib))))
    (catch Exception _ ::unreadable)))

(defn- hook-code
  "The code of hook namespace `ns-sym` and the hook namespaces it requires,
  from the config dir."
  [files ns-sym]
  (loop [todo [ns-sym] seen #{} code []]
    (if-let [[n & more] (seq todo)]
      (if (seen n)
        (recur more seen code)
        (let [fs (ns-files files n)
              requires (map required-namespaces fs)]
          (if (some #{::unreadable} requires)
            ;; which code it uses can't be told: all of the config dir's
            (sort-by (comp str first) (for [[rel f] files] [rel (slurp f)]))
            (recur (into (vec more) (apply concat requires))
                   (conj seen n)
                   (into code (map (fn [f] [n (slurp f)]) fs))))))
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
        hook-text (fn [h] (pr-str (hook h)))
        {:keys [lint-as hooks config-in-call config-in-ns ns-groups]} cfg
        {:keys [analyze-call macroexpand]} hooks
        groups (set (keep :name ns-groups))
        ;; entries keyed on a group (app-group/deft) apply to whatever
        ;; namespaces match it: no file references them by that name
        group-keyed? #(contains? groups (some-> (namespace %) symbol))
        ;; a group only reaches analysis through config-in-ns for it
        analysis-groups (into #{} (keep (fn [[g c]] (when (and (groups g) (without-linters c)) g))) config-in-ns)
        [group-syms syms] ((juxt filter remove) group-keyed?
                           (into #{} (concat (keys lint-as) (keys analyze-call) (keys macroexpand) (keys config-in-call))))
        sym-entries (into {}
                          (keep (fn [s]
                                  (let [entry [(get lint-as s)
                                               (some-> (get analyze-call s) hook)
                                               (some-> (get macroexpand s) hook)
                                               (without-linters (get config-in-call s))]]
                                    (when (some some? entry) [(str s) (digest entry)]))))
                          syms)
        [group-cfgs ns-cfgs] ((juxt filter remove) (fn [[k]] (groups k)) config-in-ns)
        ;; top-level keys of no clj-kondo meaning (:metabase/modules): only
        ;; hooks read them
        custom (into {} (filter (fn [[k]] (and (keyword? k) (namespace k)))) cfg)
        ns-entries (into {}
                         (keep (fn [[n c]] (when-let [c (without-linters c)] [(str "ns:" n) (digest c)])))
                         ns-cfgs)]
    {:custom (update-vals custom digest)
     ;; which custom keys each hooked macro's hook code mentions
     :mentions (into {}
                     (keep (fn [s]
                             (let [text (str (some-> (get analyze-call s) hook-text)
                                             (some-> (get macroexpand s) hook-text))]
                               (when (seq text)
                                 [(str s) (into #{} (filter #(str/includes? text (subs (str %) 1))) (keys custom))]))))
                     (concat (keys analyze-call) (keys macroexpand)))
     :global (digest (-> (apply dissoc cfg (keys custom))
                         (dissoc :linters :lint-as :hooks :config-in-call :config-in-ns :ns-groups :output :analysis
                                 ;; where things are, not what they say (hook code is
                                 ;; hashed by content, per symbol)
                                 :cfg-dir :classpath :use-import-dir :config-paths :auto-load-configs
                                 :skip-lint)
                         (assoc :other-hooks (dissoc hooks :analyze-call :macroexpand)
                                ;; its findings are kept as elements
                                :unresolved-namespace (get-in cfg [:linters :unresolved-namespace])
                                :ns-groups (filterv #(or (analysis-groups (:name %))
                                                         (some (fn [s] (= (str (:name %)) (namespace s))) group-syms))
                                                    ns-groups)
                                :group-keyed (into {} (for [s group-syms]
                                                        [s [(get lint-as s)
                                                            (some-> (get analyze-call s) hook)
                                                            (some-> (get macroexpand s) hook)
                                                            (without-linters (get config-in-call s))]]))
                                ;; config for a group of namespaces applies to whichever
                                ;; namespaces match it: global
                                :group-config (into {} (keep (fn [[g c]] (when-let [c (without-linters c)] [g c]))) group-cfgs))
                         (update :config-in-comment without-linters)))
     :entries (merge sym-entries ns-entries)}))

(defn diff
  "How two signatures differ: {:global-same? :changed #{entry keys}
  :custom-changed #{custom keys} :custom-readers #{macros whose hook code
  mentions a changed custom key}}."
  [a b]
  (let [differs (fn [k] (into #{}
                              (filter #(not= (get (k a) %) (get (k b) %)))
                              (concat (keys (k a)) (keys (k b)))))
        custom-changed (differs :custom)]
    {:global-same? (= (:global a) (:global b))
     :custom-changed custom-changed
     :changed (differs :entries)
     :custom-readers (into #{}
                           (keep (fn [[s ks]] (when (some custom-changed ks) s)))
                           (concat (:mentions a) (:mentions b)))}))
