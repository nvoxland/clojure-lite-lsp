(ns csl.ns-analysis
  "Answers for hooks that ask about another namespace.

  A hook can call clj-kondo.hooks-api/ns-analysis, which clj-kondo answers
  from its on-disk cache only. csl runs clj-kondo without that cache: most
  files are never analyzed in a given project (their analysis is shared),
  so a cache would say different things in different worktrees. Instead
  the answer comes from the index, as the project sees it, plus whatever
  the same batch just analyzed (csl.analyze).

  A file whose hooks asked records each answer's digest as a ref
  (\"nsa:<lang>:<ns>=<digest>\") and in its unit key, so the same file
  gets a different unit wherever the answers differ. Its analysis is
  reused only where the answers are still the same (csl.reuse), and is
  redone when they change (csl.indexer)."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [csl.config-sig :as config-sig]
   [csl.db :as db]
   [csl.kinds :as kinds]))

(set! *warn-on-reflection* true)

(def ext-langs
  "Which files answer for each language clj-kondo asks about."
  {:clj "clj" :cljs "cljs" :cljc "cljc"})

(defn- var-entry
  "A definition as ns-analysis shows it."
  [{:keys [ns name flags extra]}]
  (cond-> {:ns (symbol ns) :name (symbol name)}
    (:fixed-arities extra) (assoc :fixed-arities (:fixed-arities extra))
    (:varargs-min-arity extra) (assoc :varargs-min-arity (:varargs-min-arity extra))
    (contains? flags :private) (assoc :private true)
    (contains? flags :macro) (assoc :macro true)))

(defn answer
  "What clj-kondo's cache would say about a namespace for `lang`, from
  its var definitions `defs` ([{:ext :ns :name :lang :flags :extra}]): a
  map from language to {name entry}, or nil when no file of that kind
  defines it."
  [lang defs]
  (let [defs (filter #(= (ext-langs lang) (:ext %)) defs)
        for-lang (fn [l] (not-empty (into {} (for [d defs :when (contains? (:lang d) l)]
                                               [(symbol (:name d)) (var-entry d)]))))]
    (when (seq defs)
      (if (= :cljc lang)
        (into {} (keep (fn [l] (some->> (for-lang l) (vector l)))) [:clj :cljs])
        (some->> (for-lang lang) (hash-map lang))))))

(defn digest
  "A short digest of an answer."
  [x]
  (subs (config-sig/digest x) 0 16))

(def marker
  "The ref every unit whose hooks asked carries, to find them."
  "nsa")

(defn dep-ref [lang ns-sym d] (str "nsa:" (name lang) ":" ns-sym "=" d))

(defn parse-ref
  "[lang ns-sym digest] of a dependency ref, nil for any other."
  [s]
  (when-let [[_ l n d] (re-matches #"nsa:([^:]+):(.+)=([0-9a-f]+)" s)]
    [(keyword l) (symbol n) d]))

(defn index-answer
  "The answer for `ns-sym` and `lang` from what project `p` sees: of the
  units defining the namespace in files of that kind, the one first in
  the project's precedence."
  [c p lang ns-sym]
  (when-let [ns-id (db/query-value c "SELECT id FROM sym WHERE text = ?" (str ns-sym))]
    (let [ext (ext-langs lang)
          rows (db/query c (str "SELECT d.unit_id, pu.ord, m.text, d.lang, d.flags, d.extra,
                                        COALESCE(pf.path, (SELECT je.entry_path FROM jar_entry je
                                                           WHERE je.unit_id = d.unit_id LIMIT 1))
                                 FROM definition d
                                 JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                                 JOIN sym m ON m.id = d.name
                                 LEFT JOIN project_file pf ON pf.project_id = pu.project_id AND pf.unit_id = d.unit_id
                                 WHERE d.ns = ? AND d.kind = ?")
                         p ns-id (kinds/code :var-def))
          rows (filter (fn [[_ _ _ _ _ _ path]] (some-> ^String path (str/ends-with? (str "." ext)))) rows)
          [_ first-unit] (first (sort (map (fn [[u ord]] [ord u]) rows)))]
      (answer lang (for [[u _ nm lang-bits flags extra] (distinct rows)
                         :when (= u first-unit)]
                     {:ext ext :ns (str ns-sym) :name nm
                      :lang (kinds/bits->langs lang-bits)
                      :flags (kinds/bits->flags flags)
                      :extra (some-> extra edn/read-string)})))))
