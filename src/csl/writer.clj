(ns csl.writer
  "The daemon's writer: projects normalized elements into the index.

  It is the only process that writes analysis, so it assigns ids itself from
  in-memory counters, keeps every symbol in memory (no lookup per new
  symbol), and inserts in multi-row statements (per-statement overhead, not
  disk, limited the writer in Phase 0.2)."
  (:require
   [clojure.string :as str]
   [csl.db :as db]
   [csl.kinds :as kinds])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]
   [java.sql Connection PreparedStatement]
   [java.util ArrayList HashMap HashSet]))

(set! *warn-on-reflection* true)

(def ^:private rows-per-insert 500)

(defn- batcher
  "Collects rows for one table and inserts them `rows-per-insert` at a time
  with `INSERT OR IGNORE`."
  [^Connection c table ncols]
  (let [row-sql (str "(" (str/join "," (repeat ncols "?")) ")")
        sql (fn [n] (str "INSERT OR IGNORE INTO " table " VALUES " (str/join "," (repeat n row-sql))))
        full (delay (.prepareStatement c (sql rows-per-insert)))
        buf (ArrayList.)
        exec! (fn [^PreparedStatement ps]
                (let [i (volatile! 0)]
                  (doseq [row buf, v row]
                    (.setObject ps (int (vswap! i inc)) v)))
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

(defn base-key-hash
  "A SHA-256 over every input to a file's analysis except the clj-kondo
  config."
  ^bytes [{:keys [content-hash lang-key kondo-version options-hash]}]
  (let [md (MessageDigest/getInstance "SHA-256")
        text (fn [^String s] (.update md (.getBytes (str s "\u0000") StandardCharsets/UTF_8)))]
    (.update md ^bytes content-hash)
    (text lang-key)
    (text kondo-version)
    (.update md ^bytes options-hash)
    (.digest md)))

(defn unit-key-hash
  "The unit key: a SHA-256 over every input to a file's analysis,
  including the answers its hooks got about other namespaces (:ns-deps,
  csl.ns-analysis)."
  ^bytes [{:keys [config-hash ns-deps] :as unit-key}]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.update md (base-key-hash unit-key))
    (.update md ^bytes config-hash)
    (doseq [^String d ns-deps]
      (.update md (.getBytes (str d "\u0000") StandardCharsets/UTF_8)))
    (.digest md)))

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

(defmacro with-write-tx
  "Run body in a transaction on the writer's connection (joining an outer
  one), reloading the writer's in-memory state if it rolls back."
  [w & body]
  `(let [w# ~w]
     (try
       (db/with-tx (:c w#) ~@body)
       (catch Throwable t#
         (reload-state! w#)
         (throw t#)))))

(defn- next-id! [{:keys [ids]} k]
  (get (swap! ids update k inc) k))

(defn- intern-sym
  "The id of `text`, 0 for nil; new symbols are added through `sym-batch`."
  [{:keys [^HashMap syms] :as w} sym-batch text]
  (if (nil? text)
    0
    (or (.get syms text)
        (let [id (next-id! w :sym)]
          ((:add! sym-batch) [id text])
          (.put syms text id)
          id))))

(defn- new-searchable-name?
  "True the first time `name-id` needs a workspace-symbol search entry."
  [{:keys [^HashSet searchable]} name-id]
  (.add searchable name-id))

(defn- extra [{:keys [kind extra impl-ns]}]
  (let [m (cond-> (or extra {})
            (= :protocol-impl kind) (assoc :impl-ns impl-ns))]
    (when (seq m) (pr-str m))))

(defn- batches [c]
  {:sym (batcher c "sym" 2)
   :definition (batcher c "definition" 14)
   :doc (batcher c "doc" 2)
   :fts (batcher c "name_fts(rowid, text)" 2)
   :usage (batcher c "usage" 12)
   :file-element (batcher c "file_element" 15)
   :java-class (batcher c "java_class" 3)
   :ref (batcher c "unit_ref" 2)})

(defn- flush-all! [batches]
  (doseq [b (vals batches)] ((:flush! b))))

(defmacro ^:private with-batches
  "Bind `sym` to fresh batches for the body, closing their statements
  after."
  [[sym c] & body]
  `(let [~sym (batches ~c)]
     (try ~@body
          (finally (doseq [b# (vals ~sym)] ((:close! b#)))))))

(defn- write-elements! [w batches unit-id elements]
  (let [sym #(intern-sym w (:sym batches) %)]
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
          ((:add! (:definition batches))
           [id unit-id code ns-id name-id lang-bits nr nc ner nec
            (kinds/flags->bits (:flags el)) (sym (:defined-by el)) (sym (:defined-by-lint-as el)) (extra el)])
          (when (:doc el) ((:add! (:doc batches)) [id (:doc el)]))
          (when (and (kinds/searchable-kinds kind) (new-searchable-name? w name-id))
            ((:add! (:fts batches)) [name-id (:name el)])))

        (kinds/usage-kinds kind)
        ((:add! (:usage batches))
         [ns-id name-id unit-id nr nc lang-bits code ner nec
          (sym (:from-ns el)) (sym (:from-var el)) (kinds/flags->bits (:flags el))]))
      (when nr
        ((:add! (:file-element batches))
         [unit-id nr nc code lang-bits ner nec ns-id name-id (sym (:alias el)) (:local-id el)
          fr fc fer fec])))))

(defn- write-refs!
  "Record what a unit references (:ref elements)."
  [w batches unit-id refs]
  (doseq [{:keys [name]} refs]
    ((:add! (:ref batches)) [unit-id (intern-sym w (:sym batches) name)])))

(defn- existing-unit [c key-hash]
  (db/query-value c "SELECT unit_id FROM unit_key WHERE key = ?" key-hash))

(defn unit-id
  "The id of the unit with `unit-key`, if it has been written."
  [c unit-key]
  (existing-unit c (unit-key-hash unit-key)))

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
  config as well (csl.reuse)."
  [{:keys [c] :as w} unit-key u]
  (with-write-tx w
    (db/execute! c "INSERT OR IGNORE INTO unit_key (key, unit_id) VALUES (?, ?)" (unit-key-hash unit-key) u)))

(defn write-units!
  "Write a chunk of units in one transaction. `units` is a seq of
  [unit-key elements], where unit-key has :content-hash :lang-key
  :kondo-version :config-hash :options-hash :external?. A unit whose key
  already exists is not written again. Returns the unit ids, in order.

  Java class definitions are not unit data: jars record theirs with
  `write-java-classes!`, and they are ignored here."
  [{:keys [c] :as w} units]
  (with-write-tx w
    (with-batches [bs c]
      (let [ids (mapv (fn [[k elements]]
                      (let [key-hash (unit-key-hash k)]
                        (or (existing-unit c key-hash)
                            (let [id (next-id! w :unit)]
                              (db/execute! c "INSERT INTO unit (id, base_key, config_hash, external, created_at)
                                              VALUES (?, ?, ?, ?, ?)"
                                           id (base-key-hash k) (:config-hash k) (if (:external? k) 1 0)
                                           (System/currentTimeMillis))
                              (db/execute! c "INSERT INTO unit_key (key, unit_id) VALUES (?, ?)" key-hash id)
                              (let [{refs true others false} (group-by #(= :ref (:kind %)) elements)]
                                (write-refs! w bs id refs)
                                (write-elements! w bs id (remove #(= :java-class-def (:kind %)) others)))
                              id))))
                    units)]
        (flush-all! bs)
        ids))))

(defn write-java-classes!
  "Record jar `jar-id`'s Java classes, a seq of [class-name entry-path]."
  [{:keys [c] :as w} jar-id classes]
  (with-write-tx w
    (with-batches [bs c]
      (doseq [[class-name entry-path] classes]
        ((:add! (:java-class bs)) [(intern-sym w (:sym bs) class-name) jar-id entry-path]))
      (flush-all! bs))))
