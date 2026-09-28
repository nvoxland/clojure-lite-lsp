(ns clojure-lite-lsp.hardening-test
  "Things that go wrong in real use (DESIGN.md Phase 6)."
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.gc :as gc]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.indexer-test :refer [visible-defs sync-project! count-of]]
   [clojure-lite-lsp.kondo-config-test :refer [jar!]]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn ix [] (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)}))

(deftest a-branch-switch-the-watcher-missed
  ;; many files changed, deleted and added at once; a sync alone catches up
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a) (defn a1 [] 1)"
                        "src/app/b.clj" "(ns app.b) (defn b1 [] 1)"
                        "src/app/c.clj" "(ns app.c) (defn c1 [] 1)"})]
    (with-open [ix (ix)]
      (let [c (:c ix)
            p (sync-project! ix root)]
        (spit (io/file root "src/app/a.clj") "(ns app.a) (defn a2 [] 2)")
        (io/delete-file (io/file root "src/app/b.clj"))
        (spit (io/file root "src/app/d.clj") "(ns app.d) (defn d1 [] 1)")
        (sync-project! ix root)
        (let [defs (visible-defs c p)]
          (is (every? defs ["app.a/a2" "app.c/c1" "app.d/d1"]))
          (is (not-any? defs ["app.a/a1" "app.b/b1"])))
        (is (zero? (count-of c "project_file WHERE unit_id IS NULL")))))))

(deftest a-dependency-added-and-removed
  (let [lib (jar! {"acme/lib.clj" "(ns acme.lib) (defn from-lib [] 1)"})
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})]
    (with-open [ix (ix)]
      (let [c (:c ix)
            p (sync-project! ix root)]
        (is (not (contains? (visible-defs c p) "acme.lib/from-lib")))
        (spit (io/file root "deps.edn") (pr-str {:paths ["src"] :deps {'acme/lib {:local/root lib}}}))
        (sync-project! ix root)
        (is (contains? (visible-defs c p) "acme.lib/from-lib"))
        (spit (io/file root "deps.edn") "{:paths [\"src\"]}")
        (sync-project! ix root)
        (is (not (contains? (visible-defs c p) "acme.lib/from-lib")))))))

(deftest a-clj-kondo-upgrade-reanalyzes-everything
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a) (defn f [] 1)"})]
    (with-open [ix (ix)]
      (let [c (:c ix)
            p (sync-project! ix root)
            before (count-of c "unit")]
        (with-redefs [analyze/kondo-version "a-newer-clj-kondo"]
          (sync-project! ix root)
          (testing "every unit and jar misses and is analyzed again"
            (is (= (* 2 before) (count-of c "unit"))))
          (testing "the old analysis is garbage once nothing sees it"
            (gc/collect! (:w ix) {})
            (is (= before (count-of c "unit")))
            (is (contains? (visible-defs c p) "app.a/f"))))))))

(deftest a-full-disk-keeps-the-work
  ;; SQLite reports a full disk as SQLITE_FULL; max_page_count makes one
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a) (defn f [] 1)"})]
    (with-open [ix (ix)]
      (let [c (:c ix)
            p (sync-project! ix root)
            big (str "(ns app.big)\n" (str/join "\n" (for [i (range 3000)] (str "(defn f" i " [x] (inc x))"))))]
        (spit (io/file root "src/app/big.clj") big)
        (queue/enqueue! c p :file (str root "/src/app/big.clj") 0)
        (db/query c (str "PRAGMA max_page_count = " (+ 2 (db/query-value c "PRAGMA page_count"))))
        (testing "the batch fails, stays queued, and the indexer says to retry later"
          (is (= :retry (indexer/run-until-idle! ix)))
          (is (= 1 (queue/pending-count c p))))
        (testing "with space again, the work completes"
          (db/query c "PRAGMA max_page_count = 1073741823")
          (indexer/run-until-idle! ix)
          (is (contains? (visible-defs c p) "app.big/f2999")))))))

(deftest a-file-clj-kondo-cannot-handle-is-dropped
  ;; unlike a full disk, retrying can't help: the batch is dropped
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})]
    (with-open [ix (ix)]
      (let [c (:c ix)
            p (sync-project! ix root)]
        (queue/enqueue! c p :file (str root "/src/app/a.clj") 0)
        (with-redefs [analyze/analyze-files (fn [& _] (throw (ex-info "boom" {})))]
          (indexer/run-until-idle! ix))
        (is (zero? (queue/pending-count c p)))))))

(defn eventually [f]
  (loop [n 0] (or (f) (when (< n 3000) (Thread/sleep 20) (recur (inc n))))))

(deftest a-daemon-killed-mid-write
  ;; a real daemon process, killed with SIGKILL while indexing: the next one
  ;; resumes the queue, and every transaction is all or nothing
  (let [home (str (tu/temp-dir))
        root (project! (into {"deps.edn" "{:paths [\"src\"]}"}
                             (for [i (range 400)]
                               [(str "src/app/n" i ".clj")
                                (str "(ns app.n" i ")\n" (str/join "\n" (for [j (range 40)] (str "(defn f" j " [x] (str x " j "))"))))])))
        {:keys [db]} (home/paths home)]
    (is (= :spawned (client/ensure-daemon! {:home home})))
    (with-open [c (db/open-client db)]
      (let [p (snapshot/ensure-project! c root)
            pid (db/query-value c "SELECT pid FROM daemon WHERE id = 1")
            indexed #(db/query-value c "SELECT count(*) FROM project_file WHERE project_id = ? AND unit_id IS NOT NULL" p)]
        (queue/enqueue! c p :sync "" 1)
        (is (eventually #(pos? (indexed))) "indexing has started")
        (is (< (indexed) 400) "and not finished")
        (.destroyForcibly (.orElseThrow (java.lang.ProcessHandle/of pid)))
        (is (eventually #(not (lock/held? (:daemon-lock (home/paths home))))))
        (is (= :spawned (client/ensure-daemon! {:home home})))
        (is (eventually #(and (= 400 (indexed)) (zero? (queue/pending-count c p)))))
        (is (= "ok" (db/query-value c "PRAGMA integrity_check")))
        (testing "no half-written unit: every unit has its elements"
          (is (zero? (db/query-value c "SELECT count(*) FROM unit u WHERE NOT EXISTS
                                        (SELECT 1 FROM file_element fe WHERE fe.unit_id = u.id)"))))
        (client/request-stop! c)))))
