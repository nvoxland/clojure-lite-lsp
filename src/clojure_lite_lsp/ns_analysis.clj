(ns clojure-lite-lsp.ns-analysis
  "Answers for hooks that ask about another namespace.

  A hook can call clj-kondo.hooks-api/ns-analysis, which clj-kondo answers
  from its on-disk cache only. clojure-lite-lsp runs clj-kondo without that cache: most
  files are never analyzed in a given project (their analysis is shared),
  so a cache would say different things in different worktrees. Instead
  the answer comes from the index, as the project sees it, plus whatever
  the same batch just analyzed (clojure-lite-lsp.analyze).

  A file whose hooks asked records each answer's digest as a ref
  (\"nsa:<lang>:<ns>=<digest>\") and in its unit key, so the same file
  gets a different unit wherever the answers differ. Its analysis is
  reused only where the answers are still the same (clojure-lite-lsp.reuse), and is
  redone when they change (clojure-lite-lsp.indexer)."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def lang->ext
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
  (let [defs (filter #(= (lang->ext lang) (:ext %)) defs)
        for-lang (fn [l] (not-empty (into {} (for [d defs :when (contains? (:lang d) l)]
                                               [(symbol (:name d)) (var-entry d)]))))]
    (not-empty (into {}
                     (keep (fn [l] (some->> (for-lang l) (vector l))))
                     ;; a .cljc file answers for both
                     (if (= :cljc lang) [:clj :cljs] [lang])))))

(defn digest
  "A short digest of an answer."
  [x]
  (subs (digest/value-hex x) 0 16))

(def marker
  "The ref every unit whose hooks asked carries, to find them."
  "nsa")

(defn dep-ref
  "The ref recording that a hook asked about `ns-sym` for `lang` and was
  told what digests to `d`."
  [lang ns-sym d]
  (str "nsa:" (name lang) ":" ns-sym "=" d))

(defn parse-ref
  "[lang ns-sym digest] of a dependency ref, nil for any other."
  [s]
  (when-let [[_ l n d] (re-matches #"nsa:([^:]+):(.+)=([0-9a-f]+)" s)]
    [(keyword l) (symbol n) d]))

(defn index-answer
  "The answer for `ns-sym` and `lang` from what project `p` sees: the vars
  of the files of that kind defining the namespace (several, with in-ns)
  at the best precedence."
  [c p lang ns-sym]
  (when-let [ns-id (db/query-value c "SELECT id FROM sym WHERE text = ?" (str ns-sym))]
    (let [ext (lang->ext lang)
          suffix (str "." ext)
          rows (->> (db/query c "SELECT d.unit_id, pu.ord, m.text, d.lang, d.flags, d.extra,
                                        COALESCE(pf.path, (SELECT je.entry_path FROM jar_entry je
                                                           WHERE je.unit_id = d.unit_id LIMIT 1))
                                 FROM definition d
                                 JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                                 JOIN sym m ON m.id = d.name
                                 LEFT JOIN project_file pf ON pf.project_id = pu.project_id AND pf.unit_id = d.unit_id
                                 WHERE d.ns = ? AND d.kind = ?"
                              p ns-id (kinds/code :var-def))
                    ;; the path is the last column
                    (filter #(some-> ^String (peek %) (str/ends-with? suffix))))
          ;; every file of the namespace (in-ns) at the best precedence
          top (when (seq rows) (apply min (map second rows)))]
      (answer lang (for [[_ ord nm lang-bits flags extra] (distinct rows)
                         :when (= ord top)]
                     {:ext ext :ns (str ns-sym) :name nm
                      :lang (kinds/bits->langs lang-bits)
                      :flags (kinds/bits->flags flags)
                      :extra (some-> extra edn/read-string)})))))
