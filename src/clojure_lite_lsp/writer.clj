(ns clojure-lite-lsp.writer
  "The daemon's writer: projects normalized elements into the index.

  It is the only process that writes analysis, so it assigns ids itself from
  in-memory counters, keeps every symbol in memory (no lookup per new
  symbol), and inserts in multi-row statements (per-statement overhead, not
  disk, limited the writer in Phase 0.2)."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure.string :as str])
  (:import
   [java.sql Connection PreparedStatement]
   [java.util ArrayList HashMap HashSet]))

(set! *warn-on-reflection* true)

(def ^:private rows-per-insert 500)

(defn- batcher
  "Collects rows (values for `columns`, in order) for `table`, and inserts
  them `rows-per-insert` at a time with `INSERT OR IGNORE`."
  [^Connection c table columns]
  (let [row-sql (str "(" (db/placeholders columns) ")")
        sql (fn [n] (str "INSERT OR IGNORE INTO " table " (" (str/join ", " columns) ") VALUES "
                         (str/join "," (repeat n row-sql))))
        full (delay (.prepareStatement c (sql rows-per-insert)))
        buf (ArrayList.)
        exec! (fn [^PreparedStatement ps]
                (doseq [[i v] (map-indexed vector (apply concat buf))]
                  (.setObject ps (int (inc i)) v))
                (.executeUpdate ps)
                (.clear buf))]
    {:add! (fn [row]
             (.add buf row)
             (when (= rows-per-insert (.size buf))
               (exec! @full)))
     :flush! (fn []
               (when (pos? (.size buf))
                 (with-open [ps (.prepareStatement c (sql (.size buf)))]
                   (exec! ps))))
     ;; the full-size statement lives on the long-lived connection: closed
     ;; whether or not the write succeeded
     :close! (fn [] (when (realized? full) (.close ^PreparedStatement @full)))}))

(defn- base-key-hash
  "A SHA-256 over every input to a file's analysis except the clj-kondo
  config."
  ^bytes [{:keys [content-hash lang-key kondo-version options-hash]}]
  (digest/sha256 content-hash (str lang-key "\u0000") (str kondo-version "\u0000") options-hash))

(defn unit-key-hash
  "The unit key: a SHA-256 over every input to a file's analysis,
  including the answers its hooks got about other namespaces (:ns-deps,
  clojure-lite-lsp.ns-analysis)."
  ^bytes [{:keys [config-hash ns-deps] :as unit-key}]
  (apply digest/sha256 (base-key-hash unit-key) config-hash (map #(str % "\u0000") ns-deps)))

(defn reload-state!
  "Load ids, symbols and searchable names from the database: everything
  the writer keeps in memory."
  [{:keys [c ids ^HashMap syms ^HashSet searchable]}]
  ;; never below a high-water mark GC left (`save-high-water!`): an id
  ;; something may still hold must not come to mean a new row
  (let [top (fn [table]
              (max (db/query-value c (str "SELECT coalesce(max(id), 0) FROM " table))
                   (or (some-> (db/query-value c "SELECT value FROM meta WHERE key = ?" (str "max_id_" table)) parse-long)
                       0)))]
    (reset! ids {:sym (top "sym") :unit (top "unit") :definition (top "definition")}))
  (.clear syms)
  (doseq [[id text] (db/query c "SELECT id, text FROM sym")] (.put syms text id))
  (.clear searchable)
  (doseq [[id] (db/query c "SELECT rowid FROM name_fts")] (.add searchable id)))

(defn save-high-water!
  "Record the highest ids handed out, before deleting rows (GC)."
  [{:keys [c ids]}]
  (doseq [[k v] @ids]
    (db/execute! c "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)" (str "max_id_" (name k)) (str v))))

(defn writer
  "A writer for connection `c`, which must be the only writer of analysis."
  [c]
  (doto {:c c :ids (atom nil) :syms (HashMap.) :searchable (HashSet.)}
    (reload-state!)))

(defn call-with-write-tx
  "Call `f` in a transaction on the writer's connection (joining an outer
  one). When it rolls back, the writer's in-memory state is reloaded, by
  the outermost call: an inner one would read what's about to be rolled
  back."
  [{:keys [^Connection c] :as w} f]
  (if-not (.getAutoCommit c)
    (f)
    (try
      (db/transact c f)
      (catch Throwable t
        (try (reload-state! w) (catch Throwable x (.addSuppressed t x)))
        (throw t)))))

(defmacro with-write-tx
  "Run body in a write transaction (`call-with-write-tx`)."
  [w & body]
  `(call-with-write-tx ~w (fn [] ~@body)))

(defn- next-id! [{:keys [ids]} k]
  (get (swap! ids update k inc) k))

(defn- batches [c]
  {:sym (batcher c "sym" ["id" "text"])
   :definition (batcher c "definition" ["id" "unit_id" "kind" "ns" "name" "lang"
                                        "name_row" "name_col" "name_end_row" "name_end_col"
                                        "flags" "defined_by" "defined_by_lint_as" "extra"])
   :doc (batcher c "doc" ["definition_id" "docstring"])
   :fts (batcher c "name_fts" ["rowid" "text"])
   :usage (batcher c "usage" ["to_ns" "name" "unit_id" "name_row" "name_col" "lang" "kind"
                              "name_end_row" "name_end_col" "from_ns" "from_var" "flags"])
   :file-element (batcher c "file_element" ["unit_id" "name_row" "name_col" "kind" "lang"
                                            "name_end_row" "name_end_col" "ns" "name" "alias" "local_id"
                                            "form_row" "form_col" "form_end_row" "form_end_col"])
   :java-class (batcher c "java_class" ["name" "jar_id" "entry_path"])
   :ref (batcher c "unit_ref" ["unit_id" "ref"])})

(defn- add! [bs table row] ((:add! (bs table)) row))

(defn- with-batches
  "(f batches) with fresh batches, flushed once it returns, their
  statements closed either way."
  [c f]
  (let [bs (batches c)]
    (try
      (let [result (f bs)]
        (run! #((:flush! %)) (vals bs))
        result)
      (finally (run! #((:close! %)) (vals bs))))))

(defn- intern-sym
  "The id of `text`, 0 for nil; new symbols are added to batches `bs`."
  [{:keys [^HashMap syms] :as w} bs text]
  (if (nil? text)
    0
    (or (.get syms text)
        (let [id (next-id! w :sym)]
          (add! bs :sym [id text])
          (.put syms text id)
          id))))

(defn- claim-searchable-name!
  "True the first time `name-id` needs a workspace-symbol search entry."
  [{:keys [^HashSet searchable]} name-id]
  (.add searchable name-id))

(defn- extra [{:keys [kind extra impl-ns]}]
  (some-> (cond-> (or extra {}) (= :protocol-impl kind) (assoc :impl-ns impl-ns))
          not-empty
          pr-str))

(defn- write-elements! [w bs unit-id elements]
  (let [sym #(intern-sym w bs %)]
    (doseq [{:keys [kind lang pos form] :as el} elements
            :let [code (kinds/code kind)
                  lang-bits (kinds/langs->bits lang)
                  [nr nc ner nec] pos
                  [fr fc fer fec] form
                  ns-id (sym (:ns el))
                  name-id (sym (:name el))]]
      (cond
        (kinds/definition-kinds kind)
        (let [id (next-id! w :definition)]
          (add! bs :definition [id unit-id code ns-id name-id lang-bits nr nc ner nec
                                (kinds/flags->bits (:flags el)) (sym (:defined-by el)) (sym (:defined-by-lint-as el))
                                (extra el)])
          (when (:doc el) (add! bs :doc [id (:doc el)]))
          (when (and (kinds/searchable-kinds kind) (claim-searchable-name! w name-id))
            (add! bs :fts [name-id (:name el)])))

        (kinds/usage-kinds kind)
        (add! bs :usage [ns-id name-id unit-id nr nc lang-bits code ner nec
                         (sym (:from-ns el)) (sym (:from-var el)) (kinds/flags->bits (:flags el))]))
      (when nr
        (add! bs :file-element [unit-id nr nc code lang-bits ner nec ns-id name-id (sym (:alias el)) (:local-id el)
                                fr fc fer fec])))))

(defn- write-refs!
  "Record what a unit references (:ref elements)."
  [w bs unit-id refs]
  (doseq [{:keys [name]} refs]
    (add! bs :ref [unit-id (intern-sym w bs name)])))

(defn- unit-with-key-hash [c key-hash]
  (db/query-value c "SELECT unit_id FROM unit_key WHERE key = ?" key-hash))

(defn unit-id
  "The id of the unit with `unit-key`, if it has been written."
  [c unit-key]
  (unit-with-key-hash c (unit-key-hash unit-key)))

(defn units-with-base
  "Units analyzed from the same inputs as `unit-key` except the config:
  [[unit-id config-hash] ...], newest first."
  [c unit-key]
  (db/query c "SELECT id, config_hash FROM unit WHERE base_key = ? ORDER BY id DESC"
            (base-key-hash unit-key)))

(defn unit-refs
  "What unit `u` references: \"ns/name\" and \"ns:name\" strings."
  [c u]
  (mapv first (db/query c "SELECT s.text FROM unit_ref r JOIN sym s ON s.id = r.ref WHERE r.unit_id = ?" u)))

(defn add-unit-key!
  "Let `unit-key` find unit `u` too: its analysis holds under that key's
  config as well (clojure-lite-lsp.reuse)."
  [{:keys [c] :as w} unit-key u]
  (with-write-tx w
    (db/execute! c "INSERT OR IGNORE INTO unit_key (key, unit_id) VALUES (?, ?)" (unit-key-hash unit-key) u)))

(defn- write-unit!
  "The id of the unit with `unit-key`, writing it with `elements` unless it
  exists."
  [{:keys [c] :as w} bs unit-key elements]
  (let [key-hash (unit-key-hash unit-key)]
    (or (unit-with-key-hash c key-hash)
        (let [id (next-id! w :unit)
              [refs others] ((juxt filter remove) #(= :ref (:kind %)) elements)]
          (db/execute! c "INSERT INTO unit (id, base_key, config_hash, external, created_at) VALUES (?, ?, ?, ?, ?)"
                       id (base-key-hash unit-key) (:config-hash unit-key) (if (:external? unit-key) 1 0)
                       (System/currentTimeMillis))
          (db/execute! c "INSERT INTO unit_key (key, unit_id) VALUES (?, ?)" key-hash id)
          (write-refs! w bs id refs)
          (write-elements! w bs id (remove #(= :java-class-def (:kind %)) others))
          id))))

(defn write-units!
  "Write a chunk of units in one transaction. `units` is a seq of
  [unit-key elements], where unit-key has :content-hash :lang-key
  :kondo-version :config-hash :options-hash :external?. A unit whose key
  already exists is not written again. Returns the unit ids, in order.

  Java class definitions are not unit data: jars record theirs with
  `write-java-classes!`, and they are ignored here."
  [{:keys [c] :as w} units]
  (with-write-tx w
    (with-batches c (fn [bs] (mapv (fn [[unit-key elements]] (write-unit! w bs unit-key elements)) units)))))

(defn write-java-classes!
  "Record jar `jar-id`'s Java classes, a seq of [class-name entry-path]."
  [{:keys [c] :as w} jar-id classes]
  (with-write-tx w
    (with-batches c (fn [bs]
                      (doseq [[class-name entry-path] classes]
                        (add! bs :java-class [(intern-sym w bs class-name) jar-id entry-path]))))))
