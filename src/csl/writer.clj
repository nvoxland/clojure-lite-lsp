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
                   (exec! ps)))
               (when (realized? full) (.close ^PreparedStatement @full)))}))

(defn unit-key-hash
  "The unit key: a SHA-256 over every input to a file's analysis."
  ^bytes [{:keys [content-hash lang-key kondo-version config-hash options-hash]}]
  (let [md (MessageDigest/getInstance "SHA-256")
        text (fn [^String s] (.update md (.getBytes (str s "\u0000") StandardCharsets/UTF_8)))]
    (.update md ^bytes content-hash)
    (text lang-key)
    (text kondo-version)
    (.update md ^bytes config-hash)
    (.update md ^bytes options-hash)
    (.digest md)))

(defn reload-state!
  "Load ids, symbols and searchable names from the database: everything
  the writer keeps in memory."
  [{:keys [c ids ^HashMap syms ^HashSet searchable]}]
  (reset! ids {:sym (db/query-value c "SELECT coalesce(max(id), 0) FROM sym")
               :unit (db/query-value c "SELECT coalesce(max(id), 0) FROM unit")
               :definition (db/query-value c "SELECT coalesce(max(id), 0) FROM definition")})
  (.clear syms)
  (doseq [[id text] (db/query c "SELECT id, text FROM sym")] (.put syms text id))
  (.clear searchable)
  (doseq [[id] (db/query c "SELECT rowid FROM name_fts")] (.add searchable id)))

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
   :java-class (batcher c "java_class" 3)})

(defn- flush-all! [batches]
  (doseq [b (vals batches)] ((:flush! b))))

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

(defn- existing-unit [c key-hash]
  (db/query-value c "SELECT id FROM unit WHERE key = ?" key-hash))

(defn write-units!
  "Write a chunk of units in one transaction. `units` is a seq of
  [unit-key elements], where unit-key has :content-hash :lang-key
  :kondo-version :config-hash :options-hash :external?. A unit whose key
  already exists is not written again. Returns the unit ids, in order.

  Java class definitions are not unit data: jars record theirs with
  `write-java-classes!`, and they are ignored here."
  [{:keys [c] :as w} units]
  (with-write-tx w
    (let [bs (batches c)
          ids (mapv (fn [[k elements]]
                      (let [key-hash (unit-key-hash k)]
                        (or (existing-unit c key-hash)
                            (let [id (next-id! w :unit)]
                              (db/execute! c "INSERT INTO unit (id, key, external, created_at) VALUES (?, ?, ?, ?)"
                                           id key-hash (if (:external? k) 1 0) (System/currentTimeMillis))
                              (write-elements! w bs id (remove #(= :java-class-def (:kind %)) elements))
                              id))))
                    units)]
      (flush-all! bs)
      ids)))

(defn write-java-classes!
  "Record jar `jar-id`'s Java classes, a seq of [class-name entry-path]."
  [{:keys [c] :as w} jar-id classes]
  (with-write-tx w
    (let [bs (batches c)]
      (doseq [[class-name entry-path] classes]
        ((:add! (:java-class bs)) [(intern-sym w (:sym bs) class-name) jar-id entry-path]))
      (flush-all! bs))))
