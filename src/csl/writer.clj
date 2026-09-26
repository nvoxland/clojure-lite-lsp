(ns csl.writer
  "The daemon's writer: projects normalized elements into the index.

  It is the only process that writes analysis, so it assigns ids itself from
  in-memory counters and interns symbols through an in-memory cache, and it
  inserts in multi-row statements (per-statement overhead, not disk, limited
  the writer in Phase 0.2)."
  (:require
   [clojure.string :as str]
   [csl.db :as db]
   [csl.kinds :as kinds])
  (:import
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
                   (exec! ps)))
               (when (realized? full) (.close ^PreparedStatement @full)))}))

(defn- seed-ids [c]
  {:sym (db/query-value c "SELECT coalesce(max(id), 0) FROM sym")
   :unit (db/query-value c "SELECT coalesce(max(id), 0) FROM unit")
   :definition (db/query-value c "SELECT coalesce(max(id), 0) FROM definition")})

(defn writer
  "A writer for connection `c`, which must be the only writer of analysis."
  [c]
  {:c c
   :ids (atom (seed-ids c))
   :syms (HashMap.)
   :searchable (HashSet.)})

(defn- next-id! [{:keys [ids]} k]
  (get (swap! ids update k inc) k))

(defn- intern-sym
  "The id of `text`, 0 for nil; new symbols are added through `sym-batch`."
  [{:keys [c ^HashMap syms] :as w} sym-batch text]
  (if (nil? text)
    0
    (or (.get syms text)
        (let [id (or (db/query-value c "SELECT id FROM sym WHERE text = ?" text)
                     (let [id (next-id! w :sym)]
                       ((:add! sym-batch) [id text])
                       id))]
          (.put syms text id)
          id))))

(defn- existing-unit [c {:keys [content-hash lang-key kondo-version config-hash options-hash]}]
  (db/query-value c "SELECT id FROM unit WHERE content_hash = ? AND lang_key = ? AND kondo_version = ?
                     AND config_hash = ? AND options_hash = ?"
                  content-hash lang-key kondo-version config-hash options-hash))

(defn- insert-unit! [w {:keys [content-hash lang-key kondo-version config-hash options-hash external?]}]
  (let [id (next-id! w :unit)]
    (db/execute! (:c w) "INSERT INTO unit VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                 id content-hash lang-key kondo-version config-hash options-hash
                 (if external? 1 0) (System/currentTimeMillis))
    id))

(defn- extra [{:keys [kind extra impl-ns]}]
  (let [m (cond-> (or extra {})
            (= :protocol-impl kind) (assoc :impl-ns impl-ns))]
    (when (seq m) (pr-str m))))

(defn- new-searchable-name?
  "True the first time `name-id` needs a workspace-symbol search entry."
  [{:keys [c ^HashSet searchable]} name-id]
  (and (.add searchable name-id)
       (nil? (db/query-value c "SELECT rowid FROM name_fts WHERE rowid = ?" name-id))))

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
        (= :java-class-def kind)
        ((:add! (:java-class batches)) [name-id unit-id 0])

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
      (when (and nr (not= :java-class-def kind))
        ((:add! (:file-element batches))
         [unit-id nr nc code lang-bits ner nec ns-id name-id (sym (:alias el)) (:local-id el)
          fr fc fer fec])))))

(defn reset-caches!
  "After a rollback, forget ids and symbols that were never committed."
  [{:keys [c ids ^HashMap syms ^HashSet searchable]}]
  (reset! ids (seed-ids c))
  (.clear syms)
  (.clear searchable))

(defmacro with-write-tx
  "Run body in a transaction on the writer's connection (joining an outer
  one), resetting the writer's caches if it rolls back."
  [w & body]
  `(let [w# ~w]
     (try
       (db/with-tx (:c w#) ~@body)
       (catch Throwable t#
         (reset-caches! w#)
         (throw t#)))))

(defn write-units!
  "Write a chunk of units in one transaction. `units` is a seq of
  [unit-key elements], where unit-key has :content-hash :lang-key
  :kondo-version :config-hash :options-hash :external?. A unit whose key
  already exists is not written again. Returns the unit ids, in order."
  [{:keys [c] :as w} units]
  (with-write-tx w
    (let [batches {:sym (batcher c "sym" 2)
                   :definition (batcher c "definition" 14)
                   :doc (batcher c "doc" 2)
                   :fts (batcher c "name_fts(rowid, text)" 2)
                   :usage (batcher c "usage" 12)
                   :file-element (batcher c "file_element" 15)
                   :java-class (batcher c "java_class" 3)}
          ids (mapv (fn [[k elements]]
                      (or (existing-unit c k)
                          (let [id (insert-unit! w k)]
                            (write-elements! w batches id elements)
                            id)))
                    units)]
      (doseq [b [:sym :definition :doc :fts :usage :file-element :java-class]]
        ((:flush! (batches b))))
      ids)))
