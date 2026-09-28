(ns clojure-lite-lsp.db
  "SQLite connections and a few small JDBC helpers.

  Two kinds of connection:
  - the writer (the daemon): creates or rebuilds the schema, tuned for bulk
    writes;
  - readers (`clojure-lite-lsp lsp`): read-only, a large mmap (pages shared across
    processes through the OS page cache) and a small private page cache."
  (:require
   [clojure-lite-lsp.schema :as schema]
   [clojure.string :as str])
  (:import
   [java.sql Connection DriverManager PreparedStatement ResultSet Statement]))

(set! *warn-on-reflection* true)

(defn- bind! [^PreparedStatement ps params]
  (doseq [[i p] (map-indexed vector params)]
    (.setObject ps (int (inc i)) p)))

(defn- read-row
  "The current row of `rs`, its `n` columns as a vector."
  [^ResultSet rs n]
  (loop [i 1, row (transient [])]
    (if (> i n)
      (persistent! row)
      (recur (inc i) (conj! row (.getObject rs (int i)))))))

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
        (loop [rows (transient [])]
          (if (.next rs)
            (recur (conj! rows (read-row rs n)))
            (persistent! rows)))))))

(defn placeholders
  "\"?, ?, ?\": a parameter for each of `xs`, for an IN list."
  [xs]
  (str/join ", " (repeat (count xs) "?")))

(defn query-value
  "The first column of the first row, or nil."
  [c sql & params]
  (ffirst (apply query c sql params)))

(defn pragma!
  "Run `PRAGMA p`, whether or not it returns rows (incremental_vacuum
  returns a row per page it frees, and none when there are none)."
  [^Connection c ^String p]
  (with-open [^Statement s (.createStatement c)]
    (.execute s (str "PRAGMA " p))))

(defn transact
  "Call `f` in a transaction on `c`, committing on success; its value.
  Inside another transaction on `c`, `f` joins it."
  [^Connection c f]
  (if-not (.getAutoCommit c)
    (f)
    (do (.setAutoCommit c false)
        (let [result (try
                       (let [result (f)]
                         (.commit c)
                         result)
                       (catch Throwable t
                         ;; SQLite rolls some failures back itself (a full
                         ;; disk); cleaning up then fails too, and must not
                         ;; hide the error that matters. (Restoring autocommit
                         ;; makes sqlite-jdbc commit, which fails likewise.)
                         (try (.rollback c) (catch Throwable x (.addSuppressed t x)))
                         (try (.setAutoCommit c true) (catch Throwable x (.addSuppressed t x)))
                         (throw t)))]
          (.setAutoCommit c true)
          result))))

(defmacro with-tx
  "Run body in a transaction on `c` (`transact`)."
  [c & body]
  `(transact ~c (fn [] ~@body)))

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

(defn- connect
  "Open a connection. Writing connections begin transactions IMMEDIATE:
  taking the write lock up front is where the busy timeout applies. A
  deferred transaction that reads and then writes after another
  connection committed fails at once with SQLITE_BUSY_SNAPSHOT instead,
  and the daemon does exactly that (checks what exists, then writes)
  while clients enqueue."
  ^Connection [url & {:keys [writes?]}]
  (DriverManager/getConnection ^String url
                               (doto (java.util.Properties.)
                                 (.setProperty "transaction_mode" (if writes? "IMMEDIATE" "DEFERRED")))))

(defn- open
  "Connect to `url`, run `pragmas`, then `init`; closes the connection
  again if any of that fails."
  ^Connection [url {:keys [writes? pragmas init] :or {init identity}}]
  (let [c (connect url :writes? writes?)]
    (try
      (run! #(pragma! c %) pragmas)
      (init c)
      c
      (catch Throwable t
        (.close c)
        (throw t)))))

(defn open-writer
  "Open the index for writing, creating or rebuilding the schema as needed."
  ^Connection [path]
  (open (str "jdbc:sqlite:" path)
        {:writes? true
         :pragmas ["busy_timeout = 5000"
                   "auto_vacuum = INCREMENTAL" ; only takes effect on a new file
                   "journal_mode = WAL"
                   ;; safe in WAL mode: a crash can lose the last transactions,
                   ;; which the content-addressed index simply re-indexes
                   "synchronous = NORMAL"
                   "cache_size = -65536"]
         :init ensure-schema!}))

(defn open-client
  "Open the index for a clojure-lite-lsp lsp process's few writes
  (enqueueing, project registration, stop requests). Never creates or
  rebuilds the schema: that is the daemon's job, and a client of another
  version must not drop tables from under a running daemon.

  No journal_mode here: WAL is persistent (the daemon sets it), and
  changing it can fail at once while the daemon creates the database."
  ^Connection [path]
  (open (str "jdbc:sqlite:" path)
        {:writes? true
         ;; the daemon's transactions can take seconds (garbage
         ;; collection): waiting beats losing an edit's enqueue
         :pragmas ["busy_timeout = 30000"]}))

(defn open-reader
  "Open the index read-only for queries."
  ^Connection [path]
  (open (str "jdbc:sqlite:file:" path "?mode=ro")
        {:pragmas ["busy_timeout = 5000"
                   "query_only = 1"
                   "mmap_size = 4294967296"
                   "cache_size = -2000"]}))
