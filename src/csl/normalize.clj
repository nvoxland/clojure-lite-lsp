(ns csl.normalize
  "clj-kondo analysis -> csl elements. Pure.

  An element is a map:
    :kind        see `csl.kinds`
    :ns :name    strings (:ns nil when there is none)
    :lang        #{:clj :cljs}
    :pos         [row col end-row end-col] of the name
    :form        [row col end-row end-col] of the whole definition, or a
                 local's scope
  plus, by kind: :alias :from-ns :from-var :local-id :impl-ns :flags
  :defined-by :defined-by-lint-as :doc :extra."
  (:require
   [clj-kondo.impl.config :as kondo-config]
   [clojure.string :as str]))

(def version
  "Bump when normalization changes what it produces: it is part of every
  unit key, so all analysis is redone."
  6)

(def project-analysis-options
  {:arglists true
   :locals true
   :keywords true
   :protocol-impls true
   :java-class-definitions true
   :java-class-usages true
   :symbols true
   :var-definitions {:meta [:deprecated]}})

(def dependency-analysis-options
  {:var-usages false
   :keywords true
   :arglists true
   :protocol-impls true
   :java-class-definitions true
   :java-member-definitions false
   :var-definitions {:shallow true :meta [:deprecated]}})

(defn only-unresolved-namespace-linter
  "Every clj-kondo linter off except :unresolved-namespace, whose findings
  are the only record of calls through an unknown namespace (Phase 0.5)."
  []
  (-> (update-vals (:linters kondo-config/default-config) (constantly {:level :off}))
      (assoc :unresolved-namespace {:level :warning})))

(defn- sname [x] (some-> x str))

(defn- file-lang [filename]
  (condp re-find filename
    #"\.cljs$" #{:cljs}
    #"\.cljc$" #{:clj :cljs}
    #{:clj}))

(defn- lang [{:keys [lang filename]}]
  (if lang #{lang} (file-lang filename)))

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
    ;; for hooks that ask about a namespace (csl.ns-analysis)
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

(defn- keyword-element [external? e]
  (let [el (assoc (base (if (:reg e) :keyword-def :keyword-usage) e)
                  :ns (sname (:ns e)) :name (sname (:name e)))]
    (cond
      (:reg e) [(assoc el :defined-by (sname (:reg e)))]
      external? []
      :else [el])))

(defn- bucket->elements [external? bucket elements]
  (case bucket
    :namespace-definitions
    (map #(cond-> (assoc (base :ns-def %) :name (sname (:name %)) :form (form-pos %))
            (:doc %) (assoc :doc (:doc %)))
         elements)

    :namespace-usages
    (mapcat namespace-usage elements)

    :var-definitions
    (keep #(when (and (:name %) (not (and external? (:private %))))
             (var-definition %))
          elements)

    :var-usages
    (map var-usage elements)

    :keywords
    (mapcat #(keyword-element external? %) elements)

    :locals
    (map #(assoc (base :local %)
                 :name (sname (:name %)) :local-id (:id %)
                 :form [(:row %) (:col %) (:scope-end-row %) (:scope-end-col %)])
         elements)

    :local-usages
    (map #(assoc (base :local-usage %) :name (sname (:name %)) :local-id (:id %)) elements)

    :symbols
    (map #(assoc (base :symbol-usage %)
                 :ns (sname (:to %)) :name (sname (:name %)) :from-ns (sname (:from %)))
         elements)

    :protocol-impls
    (map #(assoc (base :protocol-impl %)
                 :ns (sname (:protocol-ns %)) :name (sname (:method-name %))
                 :impl-ns (sname (:impl-ns %)) :form (form-pos %)
                 :defined-by (sname (:defined-by %)))
         elements)

    :java-class-definitions
    (map #(assoc (base :java-class-def %) :name (sname (:class %)) :pos nil) elements)

    :java-class-usages
    (map #(assoc (base :java-class-usage %) :name (sname (:class %))) elements)

    []))

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
  (csl.config-sig)."
  [{:keys [var-usages namespace-definitions]}]
  (->> (concat (for [{:keys [filename to name]} var-usages
                     :when (and (symbol? to) name)]
                 [filename (str to "/" name)])
               (for [{:keys [filename name]} namespace-definitions :when name]
                 [filename (str "ns:" name)]))
       distinct
       (map (fn [[filename ref]] {:kind :ref :name ref :lang (file-lang filename) :filename filename}))))

(defn normalize
  "clj-kondo's result -> {filename [element ...]}."
  [{:keys [analysis findings]} {:keys [external?]}]
  (let [from-analysis (for [[bucket raws] analysis
                            raw raws
                            :let [els (bucket->elements external? bucket [raw])]
                            el els
                            :when (valid? raw el)]
                        (assoc el :filename (:filename raw)))
        from-findings (when-not external? (mapcat finding->elements findings))
        from-refs (refs analysis)]
    (into {}
          (map (fn [[filename els]]
                 (let [els (mapv #(dissoc % :filename) els)]
                   ;; only a .cljc file is analyzed once per language
                   [filename (if (str/ends-with? filename ".cljc") (merge-langs els) els)])))
          (group-by :filename (concat from-analysis from-findings from-refs)))))

(defn file-extension [filename]
  (some-> (re-find #"\.([^./:]+)$" filename) second str/lower-case))
