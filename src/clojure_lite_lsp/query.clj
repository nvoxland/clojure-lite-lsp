(ns clojure-lite-lsp.query
  "Read-only queries over the index, scoped to a project.
  Positions are clj-kondo's: 1-based rows and columns, end exclusive.

  A location is {:path :pos [row col end-row end-col]}, plus :entry for a
  file inside the jar at :path."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure-lite-lsp.sources :as sources]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

;;;; positions: [row col] and [row col end-row end-col], compared as vectors

(defn- start-of [pos] (subvec pos 0 2))
(defn- end-of [pos] (subvec pos 2 4))

(defn- pos<= [a b] (not (pos? (compare a b))))
(defn- pos< [a b] (neg? (compare a b)))

(defn- in-range?
  "Is [row col] `p` within `range` [row col end-row end-col] (its end
  excluded)?"
  [range p]
  (and (pos<= (start-of range) p) (pos< p (end-of range))))

;;;; elements

(def ^:private element-columns
  "fe.unit_id, fe.kind, fe.lang, fe.name_row, fe.name_col, fe.name_end_row, fe.name_end_col,
   ns.text, nm.text, al.text, fe.local_id, fe.form_row, fe.form_col, fe.form_end_row, fe.form_end_col")

(def ^:private element-column-count 15)

(def ^:private element-joins
  "LEFT JOIN sym ns ON ns.id = fe.ns LEFT JOIN sym nm ON nm.id = fe.name LEFT JOIN sym al ON al.id = fe.alias")

(defn- row->element [[u kind lang nr nc ner nec ns nm al local-id fr fc fer fec]]
  (cond-> {:unit-id u :kind (kinds/kind kind) :lang (kinds/bits->langs lang)
           :pos [nr nc ner nec] :ns ns :name nm}
    al (assoc :alias al)
    local-id (assoc :local-id local-id)
    fr (assoc :form [fr fc fer fec])))

(defn- file-elements
  "The file elements matching SQL `where` (over file_element fe), with
  `params`."
  [c where & params]
  (map row->element
       (apply db/query c (str "SELECT " element-columns " FROM file_element fe " element-joins " WHERE " where)
              params)))

(defn- unit-elements
  "The elements of `kinds` in unit `u`, in position order."
  [c u kinds]
  (apply file-elements c (str "fe.unit_id = ? AND fe.kind IN (" (db/placeholders kinds) ")"
                              " ORDER BY fe.name_row, fe.name_col")
         u (map kinds/code kinds)))

(defn- file-unit
  "The unit of project `p`'s file at `path`: a project file, an opened
  library file, or an extracted library source (<home>/sources/<jar hash
  hex>/<entry>, clojure-lite-lsp.sources) as its jar entry's."
  [c p path]
  (or (db/query-value c "SELECT unit_id FROM project_file WHERE project_id = ? AND path = ?" p path)
      (db/query-value c "SELECT unit_id FROM dep_file WHERE path = ?" path)
      (when-let [{:keys [jar-hash-hex entry]} (sources/entry-of path)]
        (db/query-value c "SELECT je.unit_id FROM jar j
                           JOIN project_jar pj ON pj.jar_id = j.id AND pj.project_id = ?
                           JOIN jar_entry je ON je.jar_id = j.id AND je.entry_path = ?
                           WHERE hex(j.jar_hash) = upper(?) LIMIT 1"
                        p entry jar-hash-hex))))

(defn- contains-pos?
  "Is [row col] in the element's name? `end?` counts the position just
  after it too: an editor's cursor sits between characters."
  [{:keys [pos]} row col end?]
  (or (in-range? pos [row col])
      (and end? (= [row col] (end-of pos)))))

(def ^:private max-name-rows
  "How many rows a name can span: how far back `elements-at` looks."
  5)

(defn elements-at
  "The elements of project `p`'s file at `path` whose name contains the
  position. Several when a position means different things (e.g. both
  languages of a .cljc file)."
  [c p path row col]
  (when-let [u (file-unit c p path)]
    (let [els (file-elements c "fe.unit_id = ? AND fe.name_row <= ? AND fe.name_row >= ?"
                             u row (- row max-name-rows))]
      ;; a name the cursor is in, else one it is just after
      (or (seq (filter #(contains-pos? % row col false) els))
          (filter #(contains-pos? % row col true) els)))))

;;;; locations

(def ^:private params-per-query
  "Bound parameters in one batched query, well under SQLite's limit."
  500)

(defn- conj-locations
  "`locations` {unit-id [location]} with rows [unit-id & cols] added, each
  as (->location cols)."
  [locations rows ->location]
  (reduce (fn [acc [u & cols]] (update acc u (fnil conj []) (->location cols))) locations rows))

(defn- units-locations
  "Where each of units `us` lives in project `p`: {unit-id [location]},
  its files and jar entries. One query per batch of units, not per unit:
  a popular var's references span thousands of usages in a few hundred
  files."
  [c p us]
  (reduce (fn [locations batch]
            (let [in (str "(" (db/placeholders batch) ")")]
              (-> locations
                  (conj-locations (apply db/query c (str "SELECT unit_id, path FROM project_file
                                                          WHERE project_id = ? AND unit_id IN " in)
                                         p batch)
                                  (fn [[path]] {:path path}))
                  (conj-locations (apply db/query c (str "SELECT je.unit_id, pj.path, je.entry_path, j.jar_hash
                                                          FROM project_jar pj
                                                          JOIN jar_entry je ON je.jar_id = pj.jar_id
                                                          JOIN jar j ON j.id = pj.jar_id
                                                          WHERE pj.project_id = ? AND je.unit_id IN " in)
                                         p batch)
                                  (fn [[path entry jar-hash]] {:path path :entry entry :jar-hash jar-hash}))
                  (conj-locations (apply db/query c (str "SELECT unit_id, path FROM dep_file WHERE unit_id IN " in)
                                         batch)
                                  (fn [[path]] {:path path})))))
          {}
          (partition-all params-per-query (distinct us))))

(defn- distinct-by
  "The first of `coll`'s items for each value of (f item), in order."
  [f coll]
  (first (reduce (fn [[out seen :as acc] x]
                   (let [k (f x)]
                     (if (seen k) acc [(conj out x) (conj seen k)])))
                 [[] #{}]
                 coll)))

(defn- located
  "Locations for things in project `p`: things with a :unit-id and :pos
  (placed through the unit's files and jar entries), already located ones
  with a :path, and Java classes ({:java-class}) as they are."
  [c p things]
  (let [things (distinct-by (juxt #(or (:java-class %) (:path %) (:unit-id %)) :pos) things)
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
  (if text
    (or (db/query-value c "SELECT id FROM sym WHERE text = ?" text) -1)
    0))

(defn- sym-id? [id] (and id (pos? id)))

(defn- read-extra
  "`m` with its stored :extra (EDN text, or nil) read."
  [m extra]
  (cond-> m extra (assoc :extra (edn/read-string extra))))

(defn- definitions
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
               (read-extra {:id id :unit-id u :kind kind :ns ns :name name :lang (kinds/bits->langs lang)
                            :pos [nr nc ner nec] :flags (kinds/bits->flags flags)
                            :defined-by defined-by :ord ord}
                           extra)))))

(defn- definition-at
  "The one of `defs` that is element `el`: the same unit and position."
  [{:keys [unit-id pos]} defs]
  (some #(when (and (= unit-id (:unit-id %)) (= pos (:pos %))) %) defs))

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

(def ^:private refer-all-bit (kinds/flags->bits #{:refer-all}))

(defn- refer-all-namespaces
  "The namespaces unit `u` refers all of (:refer :all, :use)."
  [c u]
  (map first (db/query c "SELECT ns.text FROM file_element fe
                          JOIN usage us ON us.to_ns = fe.ns AND us.name = fe.ns AND us.unit_id = fe.unit_id
                                        AND us.kind = fe.kind AND (us.flags & ?) != 0
                          JOIN sym ns ON ns.id = fe.ns
                          WHERE fe.unit_id = ? AND fe.kind = ?"
                       refer-all-bit u (kinds/code :ns-usage))))

(defn- referred-all-definitions
  "For an unqualified name clj-kondo resolved to a file's first refer-all
  namespace (it can't know which defines it): the definitions in any
  namespace the file refers all of."
  [c p {:keys [unit-id name alias]}]
  (when (and unit-id (not alias))
    (seq (mapcat #(without-declares (definitions c p :var-def % name)) (refer-all-namespaces c unit-id)))))

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

(def ^:private max-import-depth
  "How many potemkin re-exports deep a var is followed."
  5)

(defn- var-definitions
  "The definitions a var reference (usage or quoted symbol) resolves to."
  ([c p el] (var-definitions c p el max-import-depth))
  ([c p {:keys [ns name alias lang] :as el} depth]
   (let [defs (or (seq (without-declares (definitions c p :var-def ns name)))
                  (referred-all-definitions c p el))
         matching (or (seq (filter #(same-lang? el %) defs))
                      ;; cljs only reaches clj definitions that are macros
                      ;; (:require-macros)
                      (when (:cljs lang) (seq (filter #(and (:macro (:flags %)) (:clj (:lang %))) defs)))
                      ;; clj falls back to a cljs definition
                      (when (:clj lang) (seq defs)))]
     (or (seq (follow-imports c p el (best matching) depth))
         (when alias (best (in-lang el (definitions c p :ns-def nil ns))))))))

(def ^:private record-definers
  #{"clojure.core/defrecord" "clojure.core/deftype" "cljs.core/defrecord" "cljs.core/deftype"})

(defn- record-definitions
  "The defrecord or deftype a class name (my_app.core.R) is."
  [c p class-name]
  (when-let [[_ pkg simple] (re-matches #"(.+)\.([^.]+)" class-name)]
    (filter #(record-definers (:defined-by %))
            (definitions c p :var-def (str/replace pkg "_" "-") simple))))

(defn- record-class-name [{:keys [ns name defined-by]}]
  (when (and ns (record-definers defined-by))
    (str (munge ns) "." name)))

(defn- importers
  "[ns name] of the potemkin import-vars definitions that import var
  `ns`/`name`: calls through them are its references too."
  [c p ns name]
  (->> (db/query c "SELECT ns.text, d.extra FROM definition d INDEXED BY definition_name
                    JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                    JOIN sym ns ON ns.id = d.ns
                    WHERE d.name = ? AND d.kind = ? AND d.extra LIKE '%:imported-ns%'"
                 p (sym-id c name) (kinds/code :var-def))
       (keep (fn [[importer extra]]
               (when (= ns (:imported-ns (edn/read-string extra))) [importer name])))
       distinct))

(defn- definition-of [c p {:keys [kind unit-id local-id ns name] :as el}]
  (case kind
    (:var-usage :symbol-usage) (var-definitions c p el)
    :var-def (let [defs (definitions c p :var-def ns name)]
               ;; a declare: the real definition
               (if (declare? (definition-at el defs))
                 (or (best (remove declare? defs)) [el])
                 [el]))
    :local-usage (file-elements c "fe.unit_id = ? AND fe.kind = ? AND fe.local_id = ?"
                                unit-id (kinds/code :local) local-id)
    :local [el]
    :keyword-usage (or (best (definitions c p :keyword-def ns name)) [el])
    :keyword-def [el]
    :protocol-impl (best (definitions c p :var-def ns name))
    (:ns-usage :ns-alias) (best (in-lang el (definitions c p :ns-def nil ns)))
    :ns-def [el]
    ;; a record or type used as a class: its definition; else finding the
    ;; class's source is file work, left to the server (clojure-lite-lsp.java)
    :java-class-usage (or (seq (record-definitions c p name)) [{:java-class name}])
    []))

(defn- split-symbol
  "[ns name] of \"ns/name\", [nil name] of \"name\"."
  [s]
  (if-let [[_ ns name] (re-matches #"([^/]+)/(.+)" s)]
    [ns name]
    [nil s]))

(defn symbol-elements
  "The definitions a symbol names in project `p`: \"ns/name\" a var,
  \"ns\" a namespace, \":kw\" or \":ns/kw\" a keyword. Usable wherever
  elements at a position are."
  [c p sym]
  (if-let [[_ ns name] (re-matches #":(?:([^/]+)/)?(.+)" sym)]
    [{:kind :keyword-usage :ns ns :name name :lang #{:clj :cljs}}]
    (let [[ns name] (split-symbol sym)]
      (if ns
        (without-declares (definitions c p :var-def ns name))
        (definitions c p :ns-def nil sym)))))

(defn definition-of-elements
  "Where the things `els` mean are defined: locations."
  [c p els]
  (located c p (mapcat #(definition-of c p %) els)))

(defn definition
  "Go to definition from a position in project `p`'s file at `path`:
  locations."
  [c p path row col]
  (definition-of-elements c p (elements-at c p path row col)))

;;;; declarations

(defn- inside?
  "Is element `el`'s name inside `form-el`'s form?"
  [{:keys [form]} {:keys [pos]}]
  (and form (in-range? form (start-of pos))))

(defn- declaration-of
  "Where the file of unit `u` brings var usage `el` (or a keyword written
  through an alias) in: the alias it's written through, its :refer entry,
  or its namespace's require. nil when it isn't brought in by a require
  (the file's own vars, the require itself)."
  [c u {:keys [kind ns name alias] [_ col _ end-col] :pos :as el}]
  (when (or (#{:var-usage :symbol-usage} kind) (and (= :keyword-usage kind) alias))
    (let [;; the file's ns form in force here: the last one before it
          ns-form (last (filter #(and (same-lang? el %) (:form %) (pos<= (start-of (:form %)) (start-of (:pos el))))
                                (unit-elements c u [:ns-def])))
          in-ns-form (fn [kind pred]
                       (filter #(and (same-lang? el %) (inside? ns-form %) (not= (:pos %) (:pos el)) (pred %))
                               (unit-elements c u [kind])))
          requires #(in-ns-form :ns-usage (fn [e] (= ns (:ns e))))]
      (when (and ns-form (not (inside? ns-form el)))
        (first
         (cond
           alias (in-ns-form :ns-alias #(= alias (:alias %)))
           ;; written without its namespace: referred
           (= (- end-col col) (count name)) (concat (in-ns-form :var-usage #(= [ns name] [(:ns %) (:name %)]))
                                                    (requires))
           :else (requires)))))))

(defn declaration
  "Go to declaration from a position in project `p`'s file at `path`:
  where the file brings the name in (`declaration-of`), else its
  definition."
  [c p path row col]
  (let [els (elements-at c p path row col)
        u (file-unit c p path)]
    (or (seq (located c p (keep #(declaration-of c u %) els)))
        (definition-of-elements c p els))))

;;;; within a file

(def ^:private occurrence-kinds
  "The kinds that are occurrences of the same thing, by the element under
  the cursor's kind."
  (let [var-kinds #{:var-def :var-usage :symbol-usage}
        ns-kinds #{:ns-def :ns-usage :ns-alias}
        kw-kinds #{:keyword-def :keyword-usage}]
    {:var-def var-kinds :var-usage var-kinds :symbol-usage var-kinds
     :ns-def ns-kinds :ns-usage ns-kinds :ns-alias ns-kinds
     :keyword-def kw-kinds :keyword-usage kw-kinds}))

(def ^:private write-kinds #{:var-def :ns-def :keyword-def :local})

(defn- local-elements
  "Unit `u`'s locals and local usages matching SQL `where`."
  [c u where & params]
  (apply file-elements c (str "fe.unit_id = ? AND fe.kind IN (?, ?) AND " where)
         u (kinds/code :local) (kinds/code :local-usage) params))

(defn- local-ids
  "The ids of local `el` in every language: a .cljc file is analyzed once
  per language, and each gives the same local its own id. They share the
  binding's position."
  [c u {:keys [local-id]}]
  (into #{local-id}
        (for [{[r col] :pos} (filter (comp #{:local} :kind) (local-elements c u "fe.local_id = ?" local-id))
              el (local-elements c u "fe.name_row = ? AND fe.name_col = ?" r col)
              :when (= :local (:kind el))]
          (:local-id el))))

(defn- occurrences-of
  "The elements of unit `u` that are occurrences of `el`: the same local
  (in every language), or the same var, namespace or keyword; an alias
  and the names written through it."
  [c u {:keys [kind ns name alias] :as el}]
  (cond
    (and (= :ns-alias kind) alias)
    (file-elements c "fe.unit_id = ? AND fe.alias = ?" u (sym-id c alias))

    (#{:local :local-usage} kind)
    (let [ids (vec (local-ids c u el))]
      (apply local-elements c u (str "fe.local_id IN (" (db/placeholders ids) ")") ids))

    :else
    (when-let [ks (occurrence-kinds kind)]
      (apply file-elements c (str "fe.unit_id = ? AND fe.ns = ? AND fe.name = ? AND fe.kind IN ("
                                  (db/placeholders ks) ")")
             u (sym-id c ns) (sym-id c name) (map kinds/code ks)))))

(defn highlights
  "The occurrences, in project `p`'s file at `path`, of what's at a
  position: [{:pos :write?}], :write? for definitions and bindings."
  [c p path row col]
  (when-let [u (file-unit c p path)]
    (let [els (elements-at c p path row col)
          ;; a :keys binding is both a local and a keyword: it means the local
          els (or (seq (filter (comp #{:local :local-usage} :kind) els)) els)]
      (into []
            (comp (mapcat #(occurrences-of c u %))
                  (map (fn [{:keys [pos kind]}] {:pos pos :write? (contains? write-kinds kind)}))
                  (distinct))
            els))))

(defn resolve-symbol
  "The var definitions a symbol written as `text` (\"alias/name\",
  \"ns/name\" or \"name\") can mean in project `p`'s file at `path`, for
  text the index hasn't seen (a call being typed): through the file's
  aliases, else the file's own namespace, else clojure.core (cljs.core in
  ClojureScript). Referred vars aren't followed."
  [c p path text]
  (when-let [u (file-unit c p path)]
    (let [els (unit-elements c u [:ns-alias :ns-def])
          [qualifier name] (split-symbol text)
          own (some #(when (= :ns-def (:kind %)) (:name %)) els)
          core (if (str/ends-with? path ".cljs") "cljs.core" "clojure.core")
          candidates (if qualifier
                       [(or (some #(when (and (= :ns-alias (:kind %)) (= qualifier (:alias %))) (:ns %)) els)
                            qualifier)]
                       (filter some? [own core]))]
      (or (some #(seq (symbol-elements c p (str % "/" name))) candidates) []))))

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
  analyzed without them."
  ([c p ns name kinds] (usage-rows c p ns name kinds {}))
  ([c p ns name kinds {:keys [project-only? langs]}]
   (->> (apply db/query c (str "SELECT pf.path, u.name_row, u.name_col, u.name_end_row, u.name_end_col,
                                      u.from_ns, u.from_var, u.flags
                               FROM usage u
                               JOIN project_file pf ON pf.project_id = ? AND pf.unit_id = u.unit_id
                               WHERE u.to_ns = ? AND u.name = ? AND u.kind IN (" (db/placeholders kinds) ")"
                               (when project-only? " AND pf.ord = 0")
                               (when (seq langs) " AND (u.lang & ?) != 0"))
               p (sym-id c ns) (sym-id c name)
               (concat (map kinds/code kinds)
                       (when (seq langs) [(kinds/langs->bits langs)])))
        (mapv (fn [[path nr nc ner nec from-ns from-var flags]]
                {:path path :pos [nr nc ner nec] :from-ns-id from-ns :from-var-id from-var
                 :flag-bits flags})))))

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
  [c p {:keys [kind ns name] :as el}]
  (case kind
    :var-def [(or (definition-at el (definitions c p :var-def ns name)) el)]
    (:var-usage :symbol-usage) (or (seq (var-definitions c p el))
                                   ;; unresolved: its own name is all we know
                                   [(select-keys el [:ns :name])])
    []))

(defn- usage-langs
  "The languages whose usages can mean definition `d`: its own, and cljs
  for a clj macro (:require-macros). nil (any) when it isn't known."
  [{:keys [lang flags]}]
  (when (seq lang)
    (cond-> lang (and (:clj lang) (:macro flags)) (conj :cljs))))

(defn- referred-all-usages
  "Usages of `ns`/`name` clj-kondo attributed to another namespace: in the
  files of project `p` that refer all of `ns`, bare uses of `name` whose
  recorded namespace doesn't define it."
  [c p ns name]
  (let [ns-id (sym-id c ns)
        name-id (sym-id c name)]
    (for [[u] (db/query c "SELECT us.unit_id FROM usage us
                           JOIN project_unit pu ON pu.unit_id = us.unit_id AND pu.project_id = ?
                           WHERE us.to_ns = ? AND us.name = ? AND us.kind = ? AND (us.flags & ?) != 0"
                        p ns-id ns-id (kinds/code :ns-usage) refer-all-bit)
          el (file-elements c "fe.unit_id = ? AND fe.kind = ? AND fe.name = ? AND fe.ns != ?"
                            u (kinds/code :var-usage) name-id ns-id)
          :when (and (not (:alias el)) (empty? (definitions c p :var-def (:ns el) name)))]
      el)))

(defn- var-references [c p el include-declaration?]
  (mapcat (fn [{:keys [ns name] :as target}]
            (let [ns-id (sym-id c ns)
                  name-id (sym-id c name)
                  recursive? #(and (= ns-id (:from-ns-id %)) (= name-id (:from-var-id %)))
                  ;; the var, and the names it's also reached by
                  names (cons [ns name] (when ns (importers c p ns name)))]
              (concat
               (cond->> (for [[n nm] names
                              g (generated-names (assoc target :name nm))
                              usage (usage-rows c p n g [:var-usage :symbol-usage]  {:langs (usage-langs target)})]
                          usage)
                 ;; a call from within the definition is part of it: it
                 ;; counts only with the declaration
                 (not include-declaration?) (remove recursive?))
               (when ns (referred-all-usages c p ns name))
               ;; a record or type used as a class
               (when-let [class-name (record-class-name target)]
                 (usage-rows c p nil class-name [:java-class-usage]))
               (when (and include-declaration? (:unit-id target)) [target]))))
          (var-targets c p el)))

(defn- references-of [c p {:keys [kind ns name unit-id local-id] :as el} include-declaration?]
  (case kind
    (:var-def :var-usage :symbol-usage) (var-references c p el include-declaration?)

    :protocol-impl (concat (usage-rows c p ns name [:var-usage :symbol-usage])
                           (when include-declaration? [el]))

    (:keyword-usage :keyword-def)
    (concat (usage-rows c p ns name [:keyword-usage]  {:project-only? true})
            (when include-declaration? (definitions c p :keyword-def ns name)))

    (:local :local-usage)
    (let [ks (cond-> [:local-usage] include-declaration? (conj :local))]
      (apply file-elements c (str "fe.unit_id = ? AND fe.local_id = ? AND fe.kind IN (" (db/placeholders ks) ")")
             unit-id local-id (map kinds/code ks)))

    :java-class-usage (usage-rows c p nil name [:java-class-usage])

    ;; an alias is the file's: the names written through it there
    :ns-alias (cond->> (occurrences-of c unit-id el)
                (not include-declaration?) (remove #(= :ns-alias (:kind %))))

    (:ns-def :ns-usage)
    (let [target (if (= :ns-def kind) name ns)
          defs (definitions c p :ns-def nil target)
          ;; a namespace defined in one language only is what the other's
          ;; code means by it too (cljs :require-macros of a clj namespace)
          undefined-in (remove (set (mapcat :lang defs)) [:clj :cljs])]
      (concat (usage-rows c p target target [:ns-usage :ns-alias]  {:langs (into (set (:lang el)) undefined-in)})
              (when include-declaration? (in-lang el defs))))

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

(def ^:private defmethod-bit (kinds/flags->bits #{:defmethod}))

(defn- implementations-of [c p el]
  (mapcat (fn [{:keys [ns name defined-by]}]
            (cond
              (protocol-definers defined-by)
              ;; a method's impls; for the protocol itself, where it is
              ;; extended (extend-type, extend-protocol, reify, ...)
              (or (seq (definitions c p :protocol-impl ns name))
                  (usage-rows c p ns name [:var-usage :symbol-usage]))

              (multimethod-definers defined-by)
              (filter #(pos? (bit-and (:flag-bits %) defmethod-bit)) (usage-rows c p ns name [:var-usage]))))
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

(defn- described-definitions
  "What hover describes for element `el`: the definitions it means, with
  their ids."
  [c p el]
  (mapcat (fn [{:keys [kind ns name id] :as d}]
            (if (and (nil? id) (#{:var-def :ns-def :keyword-def} kind))
              ;; an element (e.g. the definition under the cursor): its row
              (some-> (definition-at d (definitions c p kind ns name)) vector)
              [d]))
          (definition-of c p el)))

(defn hover-of-elements
  "What to show about the things `els` mean: [{:kind :ns :name :doc
  :arglists :flags :location}]."
  [c p els]
  (into []
        (comp (mapcat #(described-definitions c p %))
              (map (fn [{:keys [id kind ns name flags extra java-class] :as d}]
                     (let [doc (when id (doc-of c id))
                           arglists (:arglist-strs extra)]
                       (cond-> {:kind kind :ns ns :name name :flags (or flags #{})
                                :location (first (located c p [d]))}
                         java-class (assoc :kind :java-class :name java-class)
                         doc (assoc :doc doc)
                         arglists (assoc :arglists arglists)))))
              (distinct))
        els))

(defn hover
  "What to show on hover at a position: for each thing it means,
  {:kind :ns :name :doc :arglists :flags :location}."
  [c p path row col]
  (hover-of-elements c p (elements-at c p path row col)))

;;;; symbols

(defn document-symbols
  "The definitions in project `p`'s file at `path`, in position order:
  [{:kind :ns :name :pos :form :defined-by :extra}]."
  [c p path]
  (when-let [u (file-unit c p path)]
    (into []
          (comp (map (fn [row]
                       (let [[element-cols [defined-by extra]] (split-at element-column-count row)]
                         (cond-> (-> (row->element element-cols)
                                     (dissoc :unit-id)
                                     (read-extra extra))
                           defined-by (assoc :defined-by defined-by)))))
                (distinct))
          (apply db/query c (str "SELECT " element-columns ", dby.text, d.extra FROM file_element fe " element-joins
                                 " LEFT JOIN definition d ON d.unit_id = fe.unit_id AND d.name_row = fe.name_row
                                                          AND d.name_col = fe.name_col AND d.kind = fe.kind
                                                          AND d.name = fe.name
                                   LEFT JOIN sym dby ON dby.id = d.defined_by
                                   WHERE fe.unit_id = ? AND fe.kind IN (" (db/placeholders kinds/searchable-kinds) ")
                                   ORDER BY fe.name_row, fe.name_col")
                 u (map kinds/code kinds/searchable-kinds)))))

(def ^:private trigram-min 3)

(defn- case-variants
  "Every way of writing `s` in upper and lower case."
  [s]
  (reduce (fn [prefixes ch]
            (for [prefix prefixes
                  v (distinct [(str/lower-case ch) (str/upper-case ch)])]
              (str prefix v)))
          [""] (map str s)))

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
         ;; ignoring case, as the trigram search does: a range per way of
         ;; writing the (one or two character) prefix
         (let [variants (case-variants query)]
           (apply db/query c (str "SELECT s.id FROM sym s
                                   WHERE (" (str/join " OR " (repeat (count variants) "(s.text >= ? AND s.text < ?)")) ")
                                   AND EXISTS (SELECT 1 FROM name_fts WHERE rowid = s.id)
                                   ORDER BY (lower(s.text) = lower(?)) DESC, length(s.text)
                                   LIMIT ?")
                  (concat (mapcat (fn [v] [v (str v "￿")]) variants) [query limit]))))))

(defn- rank-by-match
  "A sort key for definitions matching `query`: the exact name, then
  prefixes, then the project's own before dependencies, then shortest."
  [query]
  (let [q (str/lower-case query)]
    (fn [{:keys [name ord]}]
      (let [n (str/lower-case name)]
        [(if (= n q) 0 1) (if (str/starts-with? n q) 0 1) ord (count name) name]))))

(def ^:private candidates-per-result
  "Names matched per result wanted: a name can have no definition the
  project sees."
  3)

(defn workspace-symbols
  "Definitions project `p` can see whose name matches `query`, the exact
  name first, then prefixes, then the project's own before dependencies:
  [{:kind :ns :name :location :defined-by :extra}]."
  [c p query {:keys [limit] :or {limit 100}}]
  ;; a blank query matches every name: sorting them all answers nothing useful
  (when-let [ids (when-not (str/blank? query)
                   (seq (matching-name-ids c query (* candidates-per-result limit))))]
    (let [defs (->> (apply db/query c (str "SELECT d.kind, ns.text, nm.text, d.unit_id, d.name_row, d.name_col,
                                                   d.name_end_row, d.name_end_col, pu.ord, dby.text, d.extra
                                            FROM definition d INDEXED BY definition_name
                                            JOIN project_unit pu ON pu.project_id = ? AND pu.unit_id = d.unit_id
                                            LEFT JOIN sym ns ON ns.id = d.ns JOIN sym nm ON nm.id = d.name
                                            LEFT JOIN sym dby ON dby.id = d.defined_by
                                            WHERE d.name IN (" (db/placeholders ids) ")
                                            AND d.kind IN (" (db/placeholders kinds/searchable-kinds) ")")
                           (concat [p] ids (map kinds/code kinds/searchable-kinds)))
                    ;; driven by the names (definition_name), not by every
                    ;; unit the project sees
                    (map (fn [[kind ns nm u nr nc ner nec ord defined-by extra]]
                           (cond-> (read-extra {:kind (kinds/kind kind) :ns ns :name nm
                                                :unit-id u :pos [nr nc ner nec] :ord ord}
                                               extra)
                             defined-by (assoc :defined-by defined-by))))
                    (sort-by (rank-by-match query))
                    (take limit))
          ;; located all at once: one lookup per result was the slowest part left
          where (units-locations c p (map :unit-id defs))]
      (into []
            (keep (fn [{:keys [unit-id pos] :as d}]
                    (when-let [loc (first (where unit-id))]
                      (-> d (dissoc :unit-id :pos :ord) (assoc :location (assoc loc :pos pos))))))
            defs))))

;;;; call hierarchy

(defn call-hierarchy-items
  "The functions a position means, for call hierarchy: [{:ns :name
  :locations}]."
  [c p path row col]
  (into []
        (comp (mapcat #(var-targets c p %))
              (filter :unit-id)
              (map (fn [{:keys [ns name] :as d}] {:ns ns :name name :locations (located c p [d])}))
              (distinct))
        (elements-at c p path row col)))

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
        ;; two parameters per pair
        (partition-all (quot params-per-query 2) (distinct pairs))))

(defn- sym-texts
  "{id text} for sym `ids`."
  [c ids]
  (into {}
        (mapcat #(apply db/query c (str "SELECT id, text FROM sym WHERE id IN (" (db/placeholders %) ")") %))
        (partition-all params-per-query (distinct (filter sym-id? ids)))))

(defn incoming-calls
  "Who calls `ns`/`name`: [{:caller {:ns :name :locations} :calls
  [locations of the calls]}]. Top-level calls have the namespace as
  caller (:name nil). A popular var has thousands of callers: they are
  looked up and located together, not one by one."
  [c p ns name]
  (let [groups (group-by (juxt :from-ns-id :from-var-id) (usage-rows c p ns name [:var-usage]))
        texts (sym-texts c (mapcat key groups))
        ;; a caller is its var definition, or for top-level calls its ns
        ;; (keyed [0 ns-id], apart from the vars' [ns-id name-id])
        caller-key (fn [[from-ns from-var]] (if (sym-id? from-var) [from-ns from-var] [0 from-ns]))
        {ns-keys true var-keys false} (group-by #(zero? (first %)) (map caller-key (keys groups)))
        caller-defs (merge (definitions-by-ids c p :var-def var-keys)
                           (definitions-by-ids c p :ns-def ns-keys))
        where (units-locations c p (map :unit-id (mapcat val caller-defs)))]
    (mapv (fn [[[from-ns from-var :as k] calls]]
            {:caller {:ns (texts from-ns) :name (texts from-var)
                      :locations (vec (for [{:keys [unit-id pos]} (caller-defs (caller-key k))
                                            loc (where unit-id)]
                                        (assoc loc :pos pos)))}
             :calls (located c p calls)})
          groups)))

(defn- definition-form
  "The form of the var definition at `pos` in unit `u`."
  [c u pos]
  (let [[row col] pos]
    (:form (first (file-elements c "fe.unit_id = ? AND fe.name_row = ? AND fe.name_col = ? AND fe.kind = ?"
                                 u row col (kinds/code :var-def))))))

(defn- calls-in
  "The var usages inside definition `d`'s form, but its head (defn,
  defmacro, ...), which defines it rather than calls it."
  [c {:keys [unit-id pos defined-by]}]
  (when-let [[fr fc fer :as form] (definition-form c unit-id pos)]
    (->> (file-elements c "fe.unit_id = ? AND fe.kind = ? AND fe.name_row BETWEEN ? AND ?"
                        unit-id (kinds/code :var-usage) fr fer)
         (filter #(in-range? form (start-of (:pos %))))
         (remove #(and (= defined-by (str (:ns %) "/" (:name %)))
                       (= (start-of (:pos %)) [fr (inc fc)]))))))

(defn outgoing-calls
  "What `ns`/`name` calls: [{:callee {:ns :name :locations} :calls
  [locations of the calls]}], from the usages inside its definition's
  form."
  [c p ns name]
  (->> (best (definitions c p :var-def ns name))
       (mapcat #(calls-in c %))
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
