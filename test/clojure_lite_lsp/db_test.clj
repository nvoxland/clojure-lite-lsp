(ns clojure-lite-lsp.db-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.schema :as schema]
   [clojure-lite-lsp.test-util :as tu]))

(deftest open-writer-creates-schema
  (let [path (tu/temp-db-path)]
    (with-open [c (db/open-writer path)]
      (testing "WAL mode"
        (is (= "wal" (db/query-value c "PRAGMA journal_mode"))))
      (testing "every table exists"
        (is (every? (set (map first (db/query c "SELECT name FROM sqlite_master WHERE type IN ('table')")))
                    ["meta" "sym" "unit" "jar" "jar_entry" "project" "project_file" "project_jar"
                     "project_unit" "fingerprint" "classpath_memo" "definition" "usage"
                     "file_element" "java_class" "doc" "name_fts" "pending" "daemon"])))
      (testing "records the schema version"
        (is (= (str schema/version)
               (db/query-value c "SELECT value FROM meta WHERE key = 'schema_version'")))))))

(deftest open-writer-is-idempotent
  (let [path (tu/temp-db-path)]
    (with-open [c (db/open-writer path)]
      (db/execute! c "INSERT INTO sym (id, text) VALUES (1, 'kept')"))
    (with-open [c (db/open-writer path)]
      (is (= "kept" (db/query-value c "SELECT text FROM sym WHERE id = 1"))))))

(deftest schema-version-mismatch-rebuilds-the-index
  ;; The index is a cache of analysis: on a version change it is rebuilt,
  ;; not migrated.
  (let [path (tu/temp-db-path)]
    (with-open [c (db/open-writer path)]
      (db/execute! c "INSERT INTO sym (id, text) VALUES (1, 'stale')")
      (db/execute! c "UPDATE meta SET value = '-1' WHERE key = 'schema_version'"))
    (with-open [c (db/open-writer path)]
      (is (nil? (db/query-value c "SELECT text FROM sym WHERE id = 1")))
      (is (= (str schema/version)
             (db/query-value c "SELECT value FROM meta WHERE key = 'schema_version'"))))))

(deftest reader-is-read-only
  (let [path (tu/temp-db-path)]
    (with-open [_ (db/open-writer path)]
      (with-open [r (db/open-reader path)]
        (is (thrown? java.sql.SQLException
                     (db/execute! r "INSERT INTO sym (id, text) VALUES (1, 'x')")))
        (is (pos? (db/query-value r "PRAGMA mmap_size")))))))

(deftest a-transaction-that-reads-then-writes-waits-for-other-writers
  ;; the daemon checks what exists, then writes, while clients enqueue.
  ;; A deferred transaction whose snapshot went stale fails at once with
  ;; SQLITE_BUSY (the busy timeout doesn't apply); an immediate one waits.
  (let [path (tu/temp-db-path)]
    (with-open [daemon (db/open-writer path)
                client (db/open-client path)]
      (let [other-write (promise)]
        (db/with-tx daemon
          (db/query daemon "SELECT count(*) FROM sym")
          (deliver other-write (future (db/execute! client "INSERT INTO sym (id, text) VALUES (1, 'client')")))
          (Thread/sleep 100)
          (db/execute! daemon "INSERT INTO sym (id, text) VALUES (2, 'daemon')"))
        (is (= 1 (deref @other-write 10000 :timeout)))
        (is (= 2 (db/query-value daemon "SELECT count(*) FROM sym")))))))

(deftest a-failed-rollback-does-not-hide-the-error
  ;; SQLite rolls some failures back itself (a full disk); rolling back
  ;; again then fails, and must not replace the error that matters
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (db/query c (str "PRAGMA max_page_count = " (db/query-value c "PRAGMA page_count")))
    (let [e (try (db/with-tx c
                   (doseq [i (range 2000)]
                     (db/execute! c "INSERT INTO sym (id, text) VALUES (?, ?)" i (apply str i (repeat 200 "x")))))
                 nil
                 (catch Exception e e))]
      (is (instance? org.sqlite.SQLiteException e))
      (is (= org.sqlite.SQLiteErrorCode/SQLITE_FULL (.getResultCode ^org.sqlite.SQLiteException e))))))
