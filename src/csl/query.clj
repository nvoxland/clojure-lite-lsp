(ns csl.query
  "Read-only queries over the index, scoped to a project (DESIGN.md §7).
  Positions are clj-kondo's: 1-based rows and columns, end exclusive.

  A location is {:path :pos [row col end-row end-col]}, plus :entry for a
  file inside the jar at :path."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [csl.db :as db]
   [csl.kinds :as kinds]))

;;;; elements

(def ^:private element-columns
  "fe.unit_id, fe.kind, fe.lang, fe.name_row, fe.name_col, fe.name_end_row, fe.name_end_col,
   ns.text, nm.text, al.text, fe.local_id, fe.form_row, fe.form_col, fe.form_end_row, fe.form_end_col")

(def ^:private element-joins
  "LEFT JOIN sym ns ON ns.id = fe.ns LEFT JOIN sym nm ON nm.id = fe.name LEFT JOIN sym al ON al.id = fe.alias")

(defn- row->element [[u kind lang nr nc ner nec ns nm al local-id fr fc fer fec]]
  (cond-> {:unit-id u :kind (kinds/kind kind) :lang (kinds/bits->langs lang)
           :pos [nr nc ner nec] :ns ns :name nm}
    al (assoc :alias al)
    local-id (assoc :local-id local-id)
    fr (assoc :form [fr fc fer fec])))

(defn file-unit
  "The unit of project `p`'s file at `path`."
  [c p path]
  (db/query-value c "SELECT unit_id FROM project_file WHERE project_id = ? AND path = ?" p path))

(defn- contains-pos? [{[nr nc ner nec] :pos} row col]
  (and (<= nr row ner)
       (or (> row nr) (<= nc col))
       (or (< row ner) (< col nec))))

(defn elements-at
  "The elements of project `p`'s file at `path` whose name contains the
  position. Several when a position means different things (e.g. both
  languages of a .cljc file)."
  [c p path row col]
  (when-let [u (file-unit c p path)]
    (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                          " WHERE fe.unit_id = ? AND fe.name_row <= ? AND fe.name_row >= ? - 5")
                   u row row)
         (map row->element)
         (filter #(contains-pos? % row col)))))

;;;; locations

(def ^:private units-per-query 500)

(defn units-locations
  "Where each of units `us` lives in project `p`: {unit-id [location]},
  its files and jar entries. One query per batch of units, not per unit:
  a popular var's references span thousands of usages in a few hundred
  files."
  [c p us]
  (reduce (fn [acc batch]
            (let [in (str "(" (str/join "," (repeat (count batch) "?")) ")")
                  add (fn [acc rows ->loc]
                        (reduce (fn [acc [u & more]] (update acc u (fnil conj []) (->loc more))) acc rows))]
              (-> acc
                  (add (apply db/query c (str "SELECT unit_id, path FROM project_file
                                               WHERE project_id = ? AND unit_id IN " in)
                              p batch)
                       (fn [[path]] {:path path}))
                  (add (apply db/query c (str "SELECT je.unit_id, pj.path, je.entry_path FROM project_jar pj
                                               JOIN jar_entry je ON je.jar_id = pj.jar_id
                                               WHERE pj.project_id = ? AND je.unit_id IN " in)
                              p batch)
                       (fn [[path entry]] {:path path :entry entry})))))
          {}
          (partition-all units-per-query (distinct us))))

(defn unit-locations
  "Where unit `u` lives in project `p`: its files, and jar entries."
  [c p u]
  (get (units-locations c p [u]) u []))

(defn- located
  "Locations for things in project `p`: things with a :unit-id and :pos
  (placed through the unit's files and jar entries), or already located
  ones with a :path."
  [c p things]
  (let [seen (java.util.HashSet.)
        things (filterv #(.add seen [(or (:path %) (:unit-id %)) (:pos %)]) things)
        where (units-locations c p (keep #(when-not (:path %) (:unit-id %)) things))]
    (into [] (mapcat (fn [{:keys [path unit-id pos]}]
                       (if path
                         [{:path path :pos pos}]
                         (map #(assoc % :pos pos) (where unit-id)))))
          things)))

;;;; definitions

(defn- sym-id
  "The sym id of `text` (0 for nil, -1 when unknown, matching nothing)."
  [c text]
  (if (nil? text) 0 (or (db/query-value c "SELECT id FROM sym WHERE text = ?" text) -1)))

(defn definitions
  "Definitions of `kind` named `ns`/`name` that project `p` can see, best
  precedence first."
  [c p kind ns name]
  (->> (db/query c "SELECT d.unit_id, d.lang, d.name_row, d.name_col, d.name_end_row, d.name_end_col,
                           d.flags, db.text, pu.ord, d.extra, d.id
                    FROM definition d
                    JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                    LEFT JOIN sym db ON db.id = d.defined_by
                    WHERE d.ns = ? AND d.name = ? AND d.kind = ?
                    ORDER BY pu.ord"
                 p (sym-id c ns) (sym-id c name) (kinds/code kind))
       (mapv (fn [[u lang nr nc ner nec flags defined-by ord extra id]]
               (cond-> {:id id :unit-id u :kind kind :ns ns :name name :lang (kinds/bits->langs lang)
                        :pos [nr nc ner nec] :flags (kinds/bits->flags flags)
                        :defined-by defined-by :ord ord}
                 extra (assoc :extra (edn/read-string extra)))))))

(defn- best
  "The candidates with the best (lowest) precedence."
  [candidates]
  (when (seq candidates)
    (let [top (apply min (map :ord candidates))]
      (filter #(= top (:ord %)) candidates))))

(defn- declare? [{:keys [defined-by]}]
  (contains? #{"clojure.core/declare" "cljs.core/declare"} defined-by))

(defn- without-declares
  "Real definitions over `declare`s, when there are any."
  [defs]
  (or (seq (remove declare? defs)) defs))

(defn- same-lang? [el d] (boolean (some (:lang el) (:lang d))))

(declare var-definitions)

(defn- follow-imports
  "Replace potemkin/import-vars definitions with the vars they import, when
  those are visible."
  [c p el defs depth]
  (mapcat (fn [d]
            (if-let [imported (and (pos? depth) (get-in d [:extra :imported-ns]))]
              (or (seq (var-definitions c p (assoc el :ns imported :alias nil) (dec depth))) [d])
              [d]))
          defs))

(defn- var-definitions
  "The definitions a var reference (usage or quoted symbol) resolves to."
  ([c p el] (var-definitions c p el 5))
  ([c p {:keys [ns name alias lang] :as el} depth]
  (let [defs (without-declares (definitions c p :var-def ns name))
        matching (or (seq (filter #(same-lang? el %) defs))
                     ;; cljs only reaches clj definitions that are macros
                     ;; (:require-macros)
                     (when (:cljs lang) (seq (filter #(and (:macro (:flags %)) (:clj (:lang %))) defs)))
                     ;; clj falls back to a cljs definition
                     (when (:clj lang) (seq defs)))]
    (or (seq (follow-imports c p el (best matching) depth))
        (when alias (best (definitions c p :ns-def nil ns)))))))

(defn- definition-of [c p {:keys [kind unit-id local-id ns name] :as el}]
  (case kind
    (:var-usage :symbol-usage) (var-definitions c p el)
    :var-def (if (declare? (first (filter #(= (:pos %) (:pos el)) (definitions c p :var-def ns name))))
               (or (best (remove declare? (definitions c p :var-def ns name))) [el])
               [el])
    :local-usage (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                                       " WHERE fe.unit_id = ? AND fe.kind = ? AND fe.local_id = ?")
                                unit-id (kinds/code :local) local-id)
                      (map row->element))
    :local [el]
    :keyword-usage (or (best (definitions c p :keyword-def ns name)) [el])
    :keyword-def [el]
    :protocol-impl (best (definitions c p :var-def ns name))
    (:ns-usage :ns-alias) (best (definitions c p :ns-def nil ns))
    :ns-def [el]
    :java-class-usage []
    []))

(defn definition
  "Go to definition from a position in project `p`'s file at `path`:
  locations."
  [c p path row col]
  (located c p (mapcat #(definition-of c p %) (elements-at c p path row col))))

;;;; references

(defn- usage-rows
  "Usages of `ns`/`name` of the given kinds that project `p` can see, as
  located things: {:path :pos :from-ns-id :from-var-id :flag-bits} (sym
  ids and bits, not decoded: a popular var has tens of thousands of
  usages). `project-only?` keeps the project's own sources.

  Joins project_file for the path directly, which is 6x faster than
  locating units afterwards for clojure.core/let's 25k usages on Metabase.
  That is complete because only files have usages: dependencies are
  analyzed without them. (Fully analyzed dependency files, when added,
  will need their jar entries here too.)"
  [c p ns name kinds & {:keys [project-only?]}]
  (->> (db/query c (str "SELECT pf.path, u.name_row, u.name_col, u.name_end_row, u.name_end_col,
                                u.from_ns, u.from_var, u.flags
                         FROM usage u
                         JOIN project_file pf ON pf.project_id = ? AND pf.unit_id = u.unit_id
                         WHERE u.to_ns = ? AND u.name = ? AND u.kind IN ("
                        (str/join "," (map kinds/code kinds)) ")"
                        (when project-only? " AND pf.ord = 0"))
                   p (sym-id c ns) (sym-id c name))
       (mapv (fn [[path nr nc ner nec from-ns from-var flags]]
               {:path path :pos [nr nc ner nec] :from-ns-id from-ns :from-var-id from-var
                :flag-bits flags}))))

(defn- sym-text [c id]
  (when (and id (pos? id)) (db/query-value c "SELECT text FROM sym WHERE id = ?" id)))

(defn- generated-names
  "The names a definition also defines: a defrecord's and deftype's
  constructors."
  [{:keys [name defined-by]}]
  (case defined-by
    ("clojure.core/defrecord" "cljs.core/defrecord") [name (str "->" name) (str "map->" name)]
    ("clojure.core/deftype" "cljs.core/deftype") [name (str "->" name)]
    [name]))

(defn- var-targets
  "The var definitions an element means, for references and
  implementations: itself when it is one, else what it resolves to."
  [c p {:keys [kind ns name pos] :as el}]
  (case kind
    :var-def (or (seq (filter #(= pos (:pos %)) (definitions c p :var-def ns name))) [el])
    (:var-usage :symbol-usage) (or (seq (var-definitions c p el))
                                   ;; unresolved: its own name is all we know
                                   [(select-keys el [:ns :name])])
    []))

(defn- var-references [c p el include-declaration?]
  (mapcat (fn [{:keys [ns name] :as target}]
            (let [ns-id (sym-id c ns)
                  name-id (sym-id c name)
                  recursive? #(and (= ns-id (:from-ns-id %)) (= name-id (:from-var-id %)))]
              (concat
               (->> (generated-names target)
                    (mapcat #(usage-rows c p ns % [:var-usage :symbol-usage]))
                    (remove #(and (not include-declaration?) (recursive? %))))
               (when (and include-declaration? (:unit-id target)) [target]))))
          (var-targets c p el)))

(defn- references-of [c p {:keys [kind ns name unit-id local-id] :as el} include-declaration?]
  (case kind
    (:var-def :var-usage :symbol-usage) (var-references c p el include-declaration?)

    :protocol-impl (concat (usage-rows c p ns name [:var-usage :symbol-usage])
                           (when include-declaration? [el]))

    (:keyword-usage :keyword-def)
    (concat (usage-rows c p ns name [:keyword-usage] :project-only? true)
            (when include-declaration? (definitions c p :keyword-def ns name)))

    (:local :local-usage)
    (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                          " WHERE fe.unit_id = ? AND fe.local_id = ? AND fe.kind IN (?, ?)")
                   unit-id local-id (kinds/code :local-usage)
                   (kinds/code (if include-declaration? :local :local-usage)))
         (map row->element))

    (:ns-def :ns-usage :ns-alias)
    (let [target (if (= :ns-def kind) name ns)]
      (concat (usage-rows c p target target [:ns-usage :ns-alias])
              (when include-declaration? (definitions c p :ns-def nil target))))

    []))

(defn references
  "Find references from a position in project `p`'s file at `path`:
  locations."
  [c p path row col {:keys [include-declaration?]}]
  (located c p (mapcat #(references-of c p % include-declaration?) (elements-at c p path row col))))

;;;; implementations

(def ^:private protocol-definers
  #{"clojure.core/defprotocol" "cljs.core/defprotocol" "clojure.core/definterface"})

(def ^:private multimethod-definers #{"clojure.core/defmulti" "cljs.core/defmulti"})

(defn- implementations-of [c p el]
  (mapcat (fn [{:keys [ns name defined-by]}]
            (cond
              (protocol-definers defined-by)
              ;; a method's impls; for the protocol itself, where it is
              ;; extended (extend-type, extend-protocol, reify, ...)
              (or (seq (definitions c p :protocol-impl ns name))
                  (usage-rows c p ns name [:var-usage :symbol-usage]))

              (multimethod-definers defined-by)
              (filter #(:defmethod (kinds/bits->flags (:flag-bits %))) (usage-rows c p ns name [:var-usage]))))
          (var-targets c p el)))

(defn implementations
  "Find implementations from a position in project `p`'s file at `path`:
  locations."
  [c p path row col]
  (located c p (mapcat #(implementations-of c p %) (elements-at c p path row col))))

;;;; hover

(defn- doc-of [c id] (db/query-value c "SELECT docstring FROM doc WHERE definition_id = ?" id))

(defn- with-definition-rows
  "Definitions (as from `definitions`, with ids) for the targets of
  `el`: what hover describes."
  [c p el]
  (->> (definition-of c p el)
       (mapcat (fn [{:keys [kind ns name id pos] :as d}]
                 (cond
                   id [d]
                   ;; an element (e.g. the definition under the cursor): its row
                   (#{:var-def :ns-def :keyword-def} kind)
                   (filter #(= pos (:pos %)) (definitions c p kind ns name))
                   :else [d])))))

(defn hover
  "What to show on hover at a position: for each thing it means,
  {:kind :ns :name :doc :arglists :flags :location}."
  [c p path row col]
  (->> (elements-at c p path row col)
       (mapcat #(with-definition-rows c p %))
       (map (fn [{:keys [id kind ns name flags extra] :as d}]
              (let [doc (when id (doc-of c id))
                    arglists (:arglist-strs extra)]
                (cond-> {:kind kind :ns ns :name name :flags (or flags #{})
                         :location (first (located c p [d]))}
                  doc (assoc :doc doc)
                  arglists (assoc :arglists arglists)))))
       distinct
       vec))

;;;; symbols

(def ^:private document-symbol-kinds [:ns-def :var-def :keyword-def])

(defn document-symbols
  "The definitions in project `p`'s file at `path`, in position order:
  [{:kind :ns :name :pos :form}]."
  [c p path]
  (when-let [u (file-unit c p path)]
    (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                          " WHERE fe.unit_id = ? AND fe.kind IN ("
                          (str/join "," (map kinds/code document-symbol-kinds)) ")
                          ORDER BY fe.name_row, fe.name_col")
                   u)
         (mapv #(dissoc (row->element %) :unit-id)))))

(def ^:private trigram-min 3)

(defn- matching-name-ids
  "Sym ids of definition names matching `query`: a trigram substring search,
  or a prefix range for queries too short for trigrams."
  [c query limit]
  (map first
       (if (>= (count query) trigram-min)
         (db/query c "SELECT rowid FROM name_fts WHERE name_fts MATCH ? LIMIT ?"
                   (str "\"" (str/replace query "\"" "\"\"") "\"") limit)
         (db/query c "SELECT id FROM sym WHERE text >= ? AND text < ? LIMIT ?"
                   query (str query "￿") limit))))

(defn workspace-symbols
  "Definitions project `p` can see whose name matches `query`, the exact
  name first, then prefixes, then the project's own before dependencies:
  [{:kind :ns :name :location}]."
  [c p query {:keys [limit] :or {limit 100}}]
  (let [ids (matching-name-ids c query (* 20 limit))]
    (when (seq ids)
      (->> (apply db/query c (str "SELECT d.kind, ns.text, nm.text, d.unit_id,
                                    d.name_row, d.name_col, d.name_end_row, d.name_end_col, pu.ord
                             FROM definition d
                             JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                             LEFT JOIN sym ns ON ns.id = d.ns JOIN sym nm ON nm.id = d.name
                             WHERE d.kind IN (?, ?, ?) AND d.name IN ("
                            (str/join "," (repeat (count ids) "?")) ")")
                  (concat [p] (map kinds/code document-symbol-kinds) ids))
           (sort-by (fn [[_ _ nm _ _ _ _ _ ord]]
                      [(if (= nm query) 0 1) (if (str/starts-with? nm query) 0 1) ord (count nm) nm]))
           (take limit)
           (mapv (fn [[kind ns nm u nr nc ner nec]]
                   {:kind (kinds/kind kind) :ns ns :name nm
                    :location (first (located c p [{:unit-id u :pos [nr nc ner nec]}]))}))))))

;;;; call hierarchy

(defn call-hierarchy-items
  "The functions a position means, for call hierarchy: [{:ns :name
  :locations}]."
  [c p path row col]
  (->> (elements-at c p path row col)
       (mapcat #(var-targets c p %))
       (filter :unit-id)
       (map (fn [{:keys [ns name] :as d}] {:ns ns :name name :locations (located c p [d])}))
       distinct
       vec))

(defn- var-item [c p ns name]
  {:ns ns :name name
   :locations (located c p (best (definitions c p :var-def ns name)))})

(defn incoming-calls
  "Who calls `ns`/`name`: [{:caller {:ns :name :locations} :calls
  [locations of the calls]}]. Top-level calls have the namespace as
  caller (:name nil)."
  [c p ns name]
  (->> (usage-rows c p ns name [:var-usage])
       (group-by (juxt :from-ns-id :from-var-id))
       (mapv (fn [[[from-ns-id from-var-id] calls]]
               (let [from-ns (sym-text c from-ns-id)
                     from-var (sym-text c from-var-id)]
               {:caller (if from-var
                          (var-item c p from-ns from-var)
                          {:ns from-ns :name nil
                           :locations (located c p (definitions c p :ns-def nil from-ns))})
                :calls (located c p calls)})))))

(defn- form-contains? [[fr fc fer fec] [r col]]
  (and (or (> r fr) (and (= r fr) (>= col fc)))
       (or (< r fer) (and (= r fer) (< col fec)))))

(defn outgoing-calls
  "What `ns`/`name` calls: [{:callee {:ns :name :locations} :calls
  [locations of the calls]}], from the usages inside its definition's
  form."
  [c p ns name]
  (->> (best (definitions c p :var-def ns name))
       (mapcat (fn [{:keys [unit-id pos defined-by]}]
                 (let [[{:keys [form]}] (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                                                              " WHERE fe.unit_id = ? AND fe.name_row = ? AND fe.name_col = ? AND fe.kind = ?")
                                                         unit-id (first pos) (second pos) (kinds/code :var-def))
                                              (map row->element))]
                   (when form
                     (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                                           " WHERE fe.unit_id = ? AND fe.kind = ? AND fe.name_row BETWEEN ? AND ?")
                                    unit-id (kinds/code :var-usage) (first form) (nth form 2))
                          (map row->element)
                          (filter #(form-contains? form (:pos %)))
                          ;; the form's head (defn, defmacro, ...) defines it; not a call
                          (remove #(and (= defined-by (str (:ns %) "/" (:name %)))
                                        (= (take 2 (:pos %)) [(first form) (inc (second form))]))))))))
       (group-by (juxt :ns :name))
       (mapv (fn [[[callee-ns callee-name] calls]]
               {:callee (var-item c p callee-ns callee-name)
                :calls (located c p calls)}))))
