(ns csl.query
  "Read-only queries over the index, scoped to a project (DESIGN.md §7).
  Positions are clj-kondo's: 1-based rows and columns, end exclusive.

  A location is {:path :pos [row col end-row end-col]}, plus :entry for a
  file inside the jar at :path."
  (:require
   [clojure.edn :as edn]
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

(defn unit-locations
  "Where unit `u` lives in project `p`: its files, and jar entries."
  [c p u]
  (concat
   (map (fn [[path]] {:path path})
        (db/query c "SELECT path FROM project_file WHERE project_id = ? AND unit_id = ?" p u))
   (map (fn [[path entry]] {:path path :entry entry})
        (db/query c "SELECT pj.path, je.entry_path FROM project_jar pj
                     JOIN jar_entry je ON je.jar_id = pj.jar_id
                     WHERE pj.project_id = ? AND je.unit_id = ?" p u))))

(defn- located
  "Locations for things (with :unit-id and :pos) in project `p`."
  [c p things]
  (->> things
       (mapcat (fn [{:keys [unit-id pos]}]
                 (map #(assoc % :pos pos) (unit-locations c p unit-id))))
       distinct
       vec))

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
                           d.flags, db.text, pu.ord, d.extra
                    FROM definition d
                    JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                    LEFT JOIN sym db ON db.id = d.defined_by
                    WHERE d.ns = ? AND d.name = ? AND d.kind = ?
                    ORDER BY pu.ord"
                 p (sym-id c ns) (sym-id c name) (kinds/code kind))
       (mapv (fn [[u lang nr nc ner nec flags defined-by ord extra]]
               (cond-> {:unit-id u :kind kind :ns ns :name name :lang (kinds/bits->langs lang)
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
