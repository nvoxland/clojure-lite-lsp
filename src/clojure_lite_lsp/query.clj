(ns clojure-lite-lsp.query
  "Read-only queries over the index, scoped to a project (DESIGN.md §7).
  Positions are clj-kondo's: 1-based rows and columns, end exclusive.

  A location is {:path :pos [row col end-row end-col]}, plus :entry for a
  file inside the jar at :path."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.kinds :as kinds]))

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
  "The unit of project `p`'s file at `path`, or of an opened library file."
  [c p path]
  (or (db/query-value c "SELECT unit_id FROM project_file WHERE project_id = ? AND path = ?" p path)
      (db/query-value c "SELECT unit_id FROM dep_file WHERE path = ?" path)))

(defn- contains-pos?
  "Is [row col] in the element's name? `end?` counts the position just
  after it too: an editor's cursor sits between characters."
  [{[nr nc ner nec] :pos} row col end?]
  (and (<= nr row ner)
       (or (> row nr) (<= nc col))
       (or (< row ner) (< col nec) (and end? (= col nec)))))

(defn elements-at
  "The elements of project `p`'s file at `path` whose name contains the
  position. Several when a position means different things (e.g. both
  languages of a .cljc file)."
  [c p path row col]
  (when-let [u (file-unit c p path)]
    (let [els (->> (db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins
                                    " WHERE fe.unit_id = ? AND fe.name_row <= ? AND fe.name_row >= ? - 5")
                             u row row)
                   (map row->element))]
      ;; a name the cursor is in, else one it is just after
      (or (seq (filter #(contains-pos? % row col false) els))
          (filter #(contains-pos? % row col true) els)))))

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
                  (add (apply db/query c (str "SELECT je.unit_id, pj.path, je.entry_path, j.jar_hash FROM project_jar pj
                                               JOIN jar_entry je ON je.jar_id = pj.jar_id
                                               JOIN jar j ON j.id = pj.jar_id
                                               WHERE pj.project_id = ? AND je.unit_id IN " in)
                              p batch)
                       (fn [[path entry jar-hash]] {:path path :entry entry :jar-hash jar-hash}))
                  (add (apply db/query c (str "SELECT unit_id, path FROM dep_file WHERE unit_id IN " in) batch)
                       (fn [[path]] {:path path})))))
          {}
          (partition-all units-per-query (distinct us))))

(defn unit-locations
  "Where unit `u` lives in project `p`: its files, and jar entries."
  [c p u]
  (get (units-locations c p [u]) u []))

(defn- located
  "Locations for things in project `p`: things with a :unit-id and :pos
  (placed through the unit's files and jar entries), already located ones
  with a :path, and Java classes ({:java-class}) as they are."
  [c p things]
  (let [seen (java.util.HashSet.)
        things (filterv #(.add seen [(or (:java-class %) (:path %) (:unit-id %)) (:pos %)]) things)
        where (units-locations c p (keep #(when-not (or (:path %) (:java-class %)) (:unit-id %)) things))]
    (into [] (mapcat (fn [{:keys [path unit-id pos java-class]}]
                       (cond
                         java-class [{:java-class java-class}]
                         path [{:path path :pos pos}]
                         :else (map #(assoc % :pos pos) (where unit-id)))))
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

(defn- in-lang
  "The candidates in `el`'s language, else all of them."
  [el candidates]
  (or (seq (filter #(same-lang? el %) candidates)) candidates))

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
        (when alias (best (in-lang el (definitions c p :ns-def nil ns))))))))

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
    (:ns-usage :ns-alias) (best (in-lang el (definitions c p :ns-def nil ns)))
    :ns-def [el]
    ;; finding a class's source is file work, left to the server (clojure-lite-lsp.java)
    :java-class-usage [{:java-class name}]
    []))

(defn symbol-elements
  "The definitions a symbol names in project `p`: \"ns/name\" a var,
  \"ns\" a namespace. Usable wherever elements at a position are."
  [c p sym]
  (if-let [[_ ns name] (re-matches #"([^/]+)/(.+)" sym)]
    (without-declares (definitions c p :var-def ns name))
    (definitions c p :ns-def nil sym)))

(defn definition-of-elements
  "Where the things `els` mean are defined: locations."
  [c p els]
  (located c p (mapcat #(definition-of c p %) els)))

(defn definition
  "Go to definition from a position in project `p`'s file at `path`:
  locations."
  [c p path row col]
  (definition-of-elements c p (elements-at c p path row col)))

;;;; references

(defn- usage-rows
  "Usages of `ns`/`name` of the given kinds that project `p` can see, as
  located things: {:path :pos :from-ns-id :from-var-id :flag-bits} (sym
  ids and bits, not decoded: a popular var has tens of thousands of
  usages). `project-only?` keeps the project's own sources, `langs` the
  usages in those languages.

  Joins project_file for the path directly, which is 6x faster than
  locating units afterwards for clojure.core/let's 25k usages on Metabase.
  That is complete because only files have usages: dependencies are
  analyzed without them. (Fully analyzed dependency files, when added,
  will need their jar entries here too.)"
  [c p ns name kinds & {:keys [project-only? langs]}]
  (->> (apply db/query c (str "SELECT pf.path, u.name_row, u.name_col, u.name_end_row, u.name_end_col,
                                      u.from_ns, u.from_var, u.flags
                               FROM usage u
                               JOIN project_file pf ON pf.project_id = ? AND pf.unit_id = u.unit_id
                               WHERE u.to_ns = ? AND u.name = ? AND u.kind IN ("
                              (str/join "," (map kinds/code kinds)) ")"
                              (when project-only? " AND pf.ord = 0")
                              (when (seq langs) " AND (u.lang & ?) != 0"))
              p (sym-id c ns) (sym-id c name)
              (when (seq langs) [(kinds/langs->bits langs)]))
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
  [c p {:keys [kind ns name pos unit-id] :as el}]
  (case kind
    :var-def (or (seq (filter #(and (= pos (:pos %)) (= unit-id (:unit-id %))) (definitions c p :var-def ns name)))
                 [el])
    (:var-usage :symbol-usage) (or (seq (var-definitions c p el))
                                   ;; unresolved: its own name is all we know
                                   [(select-keys el [:ns :name])])
    []))

(defn- usage-langs
  "The languages whose usages can mean definition `d`: its own, and cljs
  for a clj macro (:require-macros). nil (any) when it isn't known."
  [{:keys [lang flags] :as d}]
  (when (seq lang)
    (cond-> lang (and (:clj lang) (:macro flags)) (conj :cljs))))

(defn- var-references [c p el include-declaration?]
  (mapcat (fn [{:keys [ns name] :as target}]
            (let [ns-id (sym-id c ns)
                  name-id (sym-id c name)
                  recursive? #(and (= ns-id (:from-ns-id %)) (= name-id (:from-var-id %)))]
              (concat
               (->> (generated-names target)
                    (mapcat #(usage-rows c p ns % [:var-usage :symbol-usage] :langs (usage-langs target)))
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

    :java-class-usage (usage-rows c p nil name [:java-class-usage])

    (:ns-def :ns-usage :ns-alias)
    (let [target (if (= :ns-def kind) name ns)]
      (concat (usage-rows c p target target [:ns-usage :ns-alias] :langs (:lang el))
              (when include-declaration? (in-lang el (definitions c p :ns-def nil target)))))

    []))

(defn references-of-elements
  "Where the things `els` mean are used: locations."
  [c p els {:keys [include-declaration?]}]
  (located c p (mapcat #(references-of c p % include-declaration?) els)))

(defn references
  "Find references from a position in project `p`'s file at `path`:
  locations."
  [c p path row col opts]
  (references-of-elements c p (elements-at c p path row col) opts))

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

(defn implementations-of-elements
  "Implementations of the protocols and multimethods `els` mean: locations."
  [c p els]
  (located c p (mapcat #(implementations-of c p %) els)))

(defn implementations
  "Find implementations from a position in project `p`'s file at `path`:
  locations."
  [c p path row col]
  (implementations-of-elements c p (elements-at c p path row col)))

;;;; hover

(defn- doc-of [c id] (db/query-value c "SELECT docstring FROM doc WHERE definition_id = ?" id))

(defn- with-definition-rows
  "Definitions (as from `definitions`, with ids) for the targets of
  `el`: what hover describes."
  [c p el]
  (->> (definition-of c p el)
       (mapcat (fn [{:keys [kind ns name id pos unit-id] :as d}]
                 (cond
                   id [d]
                   ;; an element (e.g. the definition under the cursor): its row
                   (#{:var-def :ns-def :keyword-def} kind)
                   (filter #(and (= pos (:pos %)) (= unit-id (:unit-id %))) (definitions c p kind ns name))
                   :else [d])))))

(declare hover-of-elements)

(defn hover
  "What to show on hover at a position: for each thing it means,
  {:kind :ns :name :doc :arglists :flags :location}."
  [c p path row col]
  (hover-of-elements c p (elements-at c p path row col)))

(defn hover-of-elements
  "What to show about the things `els` mean: [{:kind :ns :name :doc
  :arglists :flags :location}]."
  [c p els]
  (->> els
       (mapcat #(with-definition-rows c p %))
       (map (fn [{:keys [id kind ns name flags extra java-class] :as d}]
              (let [[kind name] (if java-class [:java-class java-class] [kind name])
                    doc (when id (doc-of c id))
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
  "Sym ids of up to `limit` definition names matching `query`, best first:
  the exact name, then prefixes, then shortest. A trigram substring search,
  or a prefix range for queries too short for trigrams. Ranking here keeps
  the definitions join small: joining every match was the slow part."
  [c query limit]
  (map first
       (if (>= (count query) trigram-min)
         (db/query c "SELECT s.id FROM name_fts f JOIN sym s ON s.id = f.rowid
                      WHERE name_fts MATCH ?
                      ORDER BY (s.text = ?) DESC, (substr(s.text, 1, ?) = ?) DESC, length(s.text)
                      LIMIT ?"
                   (str "\"" (str/replace query "\"" "\"\"") "\"") query (count query) query limit)
         (db/query c "SELECT s.id FROM sym s
                      WHERE s.text >= ? AND s.text < ? AND EXISTS (SELECT 1 FROM name_fts WHERE rowid = s.id)
                      ORDER BY (s.text = ?) DESC, length(s.text)
                      LIMIT ?"
                   query (str query "\uffff") query limit))))

(defn workspace-symbols
  "Definitions project `p` can see whose name matches `query`, the exact
  name first, then prefixes, then the project's own before dependencies:
  [{:kind :ns :name :location}]."
  [c p query {:keys [limit] :or {limit 100}}]
  ;; a blank query matches every name: sorting them all answers nothing useful
  (let [ids (when-not (str/blank? query) (matching-name-ids c query (* 3 limit)))]
    (when (seq ids)
      ;; driven by the names (definition_name), not by every unit the
      ;; project sees
      (->> (apply db/query c (str "SELECT d.kind, ns.text, nm.text, d.unit_id,
                                    d.name_row, d.name_col, d.name_end_row, d.name_end_col, pu.ord
                             FROM definition d INDEXED BY definition_name
                             JOIN project_unit pu ON pu.project_id = ? AND pu.unit_id = d.unit_id
                             LEFT JOIN sym ns ON ns.id = d.ns JOIN sym nm ON nm.id = d.name
                             WHERE d.name IN (" (str/join "," (repeat (count ids) "?")) ")
                             AND d.kind IN (?, ?, ?)")
                  (concat [p] ids (map kinds/code document-symbol-kinds)))
           (sort-by (fn [[_ _ nm _ _ _ _ _ ord]]
                      [(if (= nm query) 0 1) (if (str/starts-with? nm query) 0 1) ord (count nm) nm]))
           (take limit)
           ((fn [rows]
              ;; locate them all at once: one lookup per result was the
              ;; slowest part left
              (let [where (units-locations c p (map #(nth % 3) rows))]
                (vec (for [[kind ns nm u nr nc ner nec] rows
                           :let [loc (first (where u))]
                           :when loc]
                       {:kind (kinds/kind kind) :ns ns :name nm
                        :location (assoc loc :pos [nr nc ner nec])})))))))))

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

(defn- definitions-by-ids
  "Definitions of `kind` project `p` can see for [ns-id name-id] pairs,
  best precedence only: {pair [{:unit-id :pos}]}. One query per few
  hundred pairs."
  [c p kind pairs]
  (into {}
        (mapcat (fn [batch]
                  (->> (apply db/query c (str "SELECT d.ns, d.name, d.unit_id, pu.ord,
                                                      d.name_row, d.name_col, d.name_end_row, d.name_end_col
                                               FROM definition d
                                               JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                                               WHERE d.kind = ? AND (d.ns, d.name) IN (VALUES "
                                              (str/join "," (repeat (count batch) "(?, ?)")) ")")
                              p (kinds/code kind) (apply concat batch))
                       (map (fn [[n nm u ord nr nc ner nec]] {:pair [n nm] :unit-id u :ord ord :pos [nr nc ner nec]}))
                       (group-by :pair)
                       (map (fn [[pair defs]] [pair (best defs)])))))
        (partition-all 400 (distinct pairs))))

(defn- sym-texts
  "{id text} for sym `ids`."
  [c ids]
  (into {}
        (mapcat #(apply db/query c (str "SELECT id, text FROM sym WHERE id IN ("
                                        (str/join "," (repeat (count %) "?")) ")") %))
        (partition-all 500 (distinct (filter #(and % (pos? %)) ids)))))

(defn incoming-calls
  "Who calls `ns`/`name`: [{:caller {:ns :name :locations} :calls
  [locations of the calls]}]. Top-level calls have the namespace as
  caller (:name nil). A popular var has thousands of callers: they are
  looked up and located together, not one by one."
  [c p ns name]
  (let [groups (group-by (juxt :from-ns-id :from-var-id) (usage-rows c p ns name [:var-usage]))
        texts (sym-texts c (mapcat key groups))
        ;; a caller is its var definition, or for top-level calls its ns
        pair-of (fn [[from-ns from-var]] (if (and from-var (pos? from-var)) [from-ns from-var] [0 from-ns]))
        var-defs (definitions-by-ids c p :var-def (keep (fn [[_ v :as k]] (when (and v (pos? v)) (pair-of k))) (keys groups)))
        ns-defs (definitions-by-ids c p :ns-def (keep (fn [[_ v :as k]] (when-not (and v (pos? v)) (pair-of k))) (keys groups)))
        defs-of #(get (if (zero? (first %)) ns-defs var-defs) %)
        where (units-locations c p (map :unit-id (mapcat defs-of (map pair-of (keys groups)))))]
    (mapv (fn [[[from-ns from-var :as k] calls]]
            {:caller {:ns (texts from-ns) :name (texts from-var)
                      :locations (vec (for [{:keys [unit-id pos]} (defs-of (pair-of k))
                                            loc (where unit-id)]
                                        (assoc loc :pos pos)))}
             :calls (located c p calls)})
          groups)))

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

;;;; java classes

(defn java-class-jars
  "The jars on project `p`'s classpath that contain class `class-name`."
  [c p class-name]
  (mapv first (db/query c "SELECT DISTINCT pj.path FROM java_class jc
                           JOIN project_jar pj ON pj.jar_id = jc.jar_id AND pj.project_id = ?
                           WHERE jc.name = ? ORDER BY pj.ord"
                        ;; every .class is recorded, nested ones (Outer$Inner) too
                        p (sym-id c class-name))))
