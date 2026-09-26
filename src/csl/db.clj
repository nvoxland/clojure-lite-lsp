(ns csl.db
  "SQLite connections and a few small JDBC helpers.

  Two kinds of connection:
  - the writer (the daemon): creates or rebuilds the schema, tuned for bulk
    writes;
  - readers (`csl lsp`): read-only, a large mmap (pages shared across
    processes through the OS page cache) and a small private page cache."
  (:require
   [csl.schema :as schema])
  (:import
   [java.sql Connection DriverManager PreparedStatement ResultSet Statement]))

(set! *warn-on-reflection* true)

(defn- bind! [^PreparedStatement ps params]
  (loop [i 1, [p & more :as ps-left] params]
    (when (seq ps-left)
      (.setObject ps (int i) p)
      (recur (inc i) more))))

(defn execute!
  "Run a statement; returns the update count."
  [^Connection c ^String sql & params]
  (with-open [ps (.prepareStatement c sql)]
    (bind! ps params)
    (.executeUpdate ps)))

(defn query
  "Run a query; returns a vector of row vectors."
  [^Connection c ^String sql & params]
  (with-open [ps (.prepareStatement c sql)]
    (bind! ps params)
    (with-open [^ResultSet rs (.executeQuery ps)]
      (let [n (.getColumnCount (.getMetaData rs))]
        (loop [acc (transient [])]
          (if (.next rs)
            (recur (conj! acc (loop [i 1, row (transient [])]
                                (if (> i n)
                                  (persistent! row)
                                  (recur (inc i) (conj! row (.getObject rs (int i))))))))
            (persistent! acc)))))))

(defn query-value
  "The first column of the first row, or nil."
  [c sql & params]
  (ffirst (apply query c sql params)))

(defn- pragma! [^Connection c ^String p]
  (with-open [^Statement s (.createStatement c)]
    (.execute s (str "PRAGMA " p))))

(defmacro with-tx
  "Run body in a transaction on `c`, committing on success. Inside another
  `with-tx` on the same connection, body joins the outer transaction."
  [c & body]
  `(let [^Connection c# ~c]
     (if-not (.getAutoCommit c#)
       (do ~@body)
       (do
         (.setAutoCommit c# false)
         (try
           (let [r# (do ~@body)]
             (.commit c#)
             r#)
           (catch Throwable t#
             (.rollback c#)
             (throw t#))
           (finally
             (.setAutoCommit c# true)))))))

(defn- stored-version [c]
  (when (query-value c "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'meta'")
    (query-value c "SELECT value FROM meta WHERE key = 'schema_version'")))

(defn- drop-all!
  "Drop every table in place. Readers that already have the file open keep
  a consistent view, unlike deleting the file under them."
  [c]
  ;; virtual tables first: dropping one drops its shadow tables
  (doseq [[n] (query c "SELECT name FROM sqlite_master
                        WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
                        ORDER BY sql LIKE 'CREATE VIRTUAL%' DESC")]
    (when (query-value c "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" n)
      (execute! c (str "DROP TABLE \"" n "\"")))))

(defn- ensure-schema! [c]
  (let [v (stored-version c)]
    (when-not (= v (str schema/version))
      (with-tx c
        (when v (drop-all! c))
        (doseq [stmt schema/ddl] (execute! c stmt))
        (execute! c "INSERT INTO meta (key, value) VALUES ('schema_version', ?)"
                  (str schema/version))))))

(defn- connect ^Connection [url]
  (DriverManager/getConnection ^String url))

(defn open-writer
  "Open the index for writing, creating or rebuilding the schema as needed."
  ^Connection [path]
  (let [c (connect (str "jdbc:sqlite:" path))]
    (pragma! c "busy_timeout = 5000")
    (pragma! c "auto_vacuum = INCREMENTAL") ; only takes effect on a new file
    (pragma! c "journal_mode = WAL")
    ;; safe in WAL mode: a crash can lose the last transactions, which the
    ;; content-addressed index simply re-indexes
    (pragma! c "synchronous = NORMAL")
    (pragma! c "cache_size = -65536")
    (ensure-schema! c)
    c))

(defn open-reader
  "Open the index read-only for queries."
  ^Connection [path]
  (let [c (connect (str "jdbc:sqlite:file:" path "?mode=ro"))]
    (pragma! c "busy_timeout = 5000")
    (pragma! c "query_only = 1")
    (pragma! c "mmap_size = 4294967296")
    (pragma! c "cache_size = -2000")
    c))
