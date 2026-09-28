(ns clojure-lite-lsp.normalize
  "clj-kondo analysis -> clojure-lite-lsp elements. Pure.

  An element is a map:
    :kind        see `clojure-lite-lsp.kinds`
    :ns :name    strings (:ns nil when there is none)
    :lang        #{:clj :cljs}
    :pos         [row col end-row end-col] of the name
    :form        [row col end-row end-col] of the whole definition, or a
                 local's scope
  plus, by kind: :alias :from-ns :from-var :local-id :impl-ns :flags
  :defined-by :defined-by-lint-as :doc :extra."
  (:require
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def version
  "Bump when normalization changes what it produces: it is part of every
  unit key, so all analysis is redone."
  8)

(defn- sname [x] (some-> x str))

(defn- file-lang [filename]
  (condp re-find filename
    #"\.cljs$" #{:cljs}
    #"\.cljc$" #{:clj :cljs}
    ;; data: its symbols can mean either language's vars
    #"\.edn$" #{:clj :cljs}
    #{:clj}))

(defn- lang [{:keys [lang filename]}]
  (if (and lang (not= :edn lang)) #{lang} (file-lang filename)))

(defn- name-pos
  "The element's name position, falling back to the element's own."
  [{:keys [row col end-row end-col name-row name-col name-end-row name-end-col]}]
  [(or name-row row) (or name-col col) (or name-end-row end-row) (or name-end-col end-col)])

(defn- form-pos [{:keys [row col end-row end-col]}]
  (when row [row col end-row end-col]))

(defn- base [kind e]
  {:kind kind :lang (lang e) :pos (name-pos e)})

(defn- fully-qualified?
  "Written as ns/name: the name's span is exactly as long as `ns/name`."
  [{:keys [to name name-col name-end-col]}]
  (and name-col name-end-col
       (= (count (str to "/" name)) (- name-end-col name-col))))

(defn- flags [& pairs]
  (into #{} (keep (fn [[flag on?]] (when on? flag))) (partition 2 pairs)))

(defn- var-definition [e]
  (cond-> (assoc (base :var-def e)
                 :ns (sname (:ns e)) :name (sname (:name e))
                 :form (form-pos e)
                 :flags (flags :private (:private e) :macro (:macro e) :deprecated (:deprecated e))
                 :defined-by (sname (:defined-by e)))
    (:defined-by->lint-as e) (assoc :defined-by-lint-as (sname (:defined-by->lint-as e)))
    (:doc e) (assoc :doc (:doc e))
    (:arglist-strs e) (assoc-in [:extra :arglist-strs] (vec (:arglist-strs e)))
    ;; for hooks that ask about a namespace (clojure-lite-lsp.ns-analysis)
    (:fixed-arities e) (assoc-in [:extra :fixed-arities] (set (:fixed-arities e)))
    (:varargs-min-arity e) (assoc-in [:extra :varargs-min-arity] (:varargs-min-arity e))
    ;; potemkin/import-vars: where the var really lives
    (:imported-ns e) (assoc-in [:extra :imported-ns] (sname (:imported-ns e)))))

(defn- var-usage [e]
  (cond-> (assoc (base :var-usage e)
                 :ns (sname (:to e)) :name (sname (:name e))
                 :from-ns (sname (:from e)) :from-var (sname (:from-var e))
                 :flags (flags :defmethod (:defmethod e)
                               :fully-qualified (and (not (:alias e)) (fully-qualified? e))))
    (:alias e) (assoc :alias (sname (:alias e)))))

(defn- namespace-usage [e]
  (let [usage (assoc (base :ns-usage e)
                     :ns (sname (:to e)) :name (sname (:to e)) :from-ns (sname (:from e)))]
    (cond-> [usage]
      (:alias e) (conj (assoc usage
                              :kind :ns-alias
                              :alias (sname (:alias e))
                              :pos [(:alias-row e) (:alias-col e) (:alias-end-row e) (:alias-end-col e)])))))

(defn- keyword-element [external? {:keys [reg] :as e}]
  ;; a dependency's keywords matter only where they're registered
  (when (or reg (not external?))
    [(cond-> (assoc (base (if reg :keyword-def :keyword-usage) e)
                    :ns (sname (:ns e)) :name (sname (:name e)))
       ;; ::al/k: a use of the alias
       (:alias e) (assoc :alias (sname (:alias e)))
       reg (assoc :defined-by (sname reg)))]))

(defn- raw->elements
  "The elements of one entry `e` of clj-kondo's analysis `bucket`."
  [external? bucket e]
  (case bucket
    :namespace-definitions
    [(cond-> (assoc (base :ns-def e) :name (sname (:name e)) :form (form-pos e))
       (:doc e) (assoc :doc (:doc e)))]

    :namespace-usages
    (namespace-usage e)

    :var-definitions
    (when (and (:name e) (not (and external? (:private e))))
      [(var-definition e)])

    :var-usages
    [(var-usage e)]

    :keywords
    (keyword-element external? e)

    :locals
    [(assoc (base :local e)
            :name (sname (:name e)) :local-id (:id e)
            :form [(:row e) (:col e) (:scope-end-row e) (:scope-end-col e)])]

    :local-usages
    [(assoc (base :local-usage e) :name (sname (:name e)) :local-id (:id e))]

    :symbols
    ;; a quoted symbol of a namespace that isn't required (or in .edn) has
    ;; no :to: the namespace written in it
    [(assoc (base :symbol-usage e)
            :ns (sname (or (:to e) (some-> (:symbol e) namespace)))
            :name (sname (:name e)) :from-ns (sname (:from e)))]

    :protocol-impls
    [(assoc (base :protocol-impl e)
            :ns (sname (:protocol-ns e)) :name (sname (:method-name e))
            :impl-ns (sname (:impl-ns e)) :form (form-pos e)
            :defined-by (sname (:defined-by e)))]

    :java-class-definitions
    [(assoc (base :java-class-def e) :name (sname (:class e)) :pos nil)]

    :java-class-usages
    [(assoc (base :java-class-usage e) :name (sname (:class e)))]

    nil))

(defn- finding->elements [{:keys [type filename row col end-row end-col] :as f}]
  (when (= :unresolved-namespace type)
    [{:kind :var-usage
      :filename filename
      :lang (file-lang filename)
      :ns (sname (:ns f)) :alias (sname (:ns f)) :name (sname (:name f))
      :pos [row col end-row end-col]
      :flags #{:unresolved :fully-qualified}}]))

(defn- valid?
  "Elements need a complete name position and one clj-kondo didn't derive,
  except Java class definitions, which have no positions."
  [raw el]
  (or (= :java-class-def (:kind el))
      (and (every? some? (:pos el))
           (not (:derived-location raw))
           (not (:derived-name-location raw)))))

(def ^:private lang-specific-keys
  "Keys that differ between a .cljc file's clj and cljs analysis of the
  same element (e.g. :defined-by clojure.core/defn vs cljs.core/defn)."
  [:lang :defined-by :defined-by-lint-as])

(defn- merge-langs
  "clj-kondo analyzes a .cljc file once per language. Merge elements that
  are the same apart from `lang-specific-keys` into the first one seen,
  with the union of the languages."
  [els]
  (let [k #(apply dissoc % lang-specific-keys)
        groups (group-by k els)]
    (->> els
         (map k)
         distinct
         (mapv (fn [key]
                 (let [[first-el :as group] (groups key)]
                   (assoc first-el :lang (into #{} (mapcat :lang) group))))))))

(defn- refs
  "What each file references, as :ref elements named \"ns/name\" for every
  var it calls (including calls inside hook expansions, which have no
  usable position and are otherwise dropped) and \"ns:name\" for its own
  namespaces. A file's analysis depends only on the config for these
  (clojure-lite-lsp.config-sig)."
  [{:keys [var-usages namespace-definitions]}]
  (->> (concat (for [{:keys [filename to name]} var-usages
                     :when (and (symbol? to) name)]
                 [filename (str to "/" name)])
               (for [{:keys [filename name]} namespace-definitions :when name]
                 [filename (str "ns:" name)]))
       distinct
       (map (fn [[filename ref]] {:kind :ref :name ref :lang (file-lang filename) :filename filename}))))

(defn- before? [[r1 c1] [r2 c2]] (or (< r1 r2) (and (= r1 r2) (< c1 c2))))

(defn- mark-refer-alls
  "Flag the ns-usages of `els` (one file's) whose names are all referred:
  a :refer-all finding follows its namespace in the require; a :use
  finding precedes the namespaces of its clause."
  [els findings]
  (let [usages (sort-by :pos (filter #(= :ns-usage (:kind %)) els))
        at #(vector (:row %) (:col %))
        refer-all (set (keep (fn [f] (last (filter #(before? (take 2 (:pos %)) (at f)) usages)))
                             (filter #(= :refer-all (:type %)) findings)))
        use-from (->> (filter #(= :use (:type %)) findings) (map at) sort first)
        refer-all? #(or (refer-all %)
                        (and use-from (before? use-from (take 2 (:pos %)))))]
    (mapv #(if (and (= :ns-usage (:kind %)) (refer-all? %)) (update % :flags (fnil conj #{}) :refer-all) %) els)))

(defn normalize
  "clj-kondo's result -> {filename [element ...]}."
  [{:keys [analysis findings]} {:keys [external?]}]
  (let [from-analysis (for [[bucket raws] analysis
                            raw raws
                            el (raw->elements external? bucket raw)
                            :when (valid? raw el)]
                        (assoc el :filename (:filename raw)))
        from-findings (when-not external? (mapcat finding->elements findings))
        findings-by-file (group-by :filename findings)]
    (into {}
          (map (fn [[filename els]]
                 (let [els (-> (mapv #(dissoc % :filename) els)
                               (mark-refer-alls (findings-by-file filename)))]
                   ;; only a .cljc file is analyzed once per language
                   [filename (cond-> els (str/ends-with? filename ".cljc") merge-langs)])))
          (group-by :filename (concat from-analysis from-findings (refs analysis))))))

