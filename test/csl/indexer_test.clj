(ns csl.indexer-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [csl.classpath-test :refer [project!]]
   [csl.db :as db]
   [csl.indexer :as indexer]
   [csl.kondo-config-test :refer [jar!]]
   [csl.queue :as queue]
   [csl.snapshot :as snapshot]
   [csl.test-util :as tu]))

(defn fixture
  "A project using a local jar and an external source dir, as deps.edn
  :local/root deps."
  []
  (let [lib (jar! {"acme/lib.clj" "(ns acme.lib) (defn from-jar [] 1) (defn- hidden [] 2)"})
        ext (project! {"deps.edn" "{:paths [\"src\"]}"
                       "src/ext/core.clj" "(ns ext.core) (defn from-ext [] 1)"})
        root (project! {"deps.edn" (pr-str {:paths ["src"]
                                            :deps {'acme/lib {:local/root lib}
                                                   'ext/core {:local/root ext}}})
                        "src/app/a.clj" "(ns app.a (:require [acme.lib :as lib])) (defn fa [] (lib/from-jar))"
                        "src/app/b.clj" "(ns app.b) (defn fb [] 1)"})]
    {:root root :lib lib :ext ext}))

(defn visible-defs
  "The var definitions project `p` can see, as #{\"ns/name\"}."
  [c p]
  (set (map first (db/query c "SELECT n.text || '/' || m.text FROM definition d
                               JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                               JOIN sym n ON n.id = d.ns JOIN sym m ON m.id = d.name
                               WHERE d.kind = 1" p))))

(defn count-of [c table] (db/query-value c (str "SELECT count(*) FROM " table)))

(defn sync-project! [ix root]
  (let [c (:c ix)
        p (snapshot/ensure-project! c root)]
    (queue/enqueue! c p :sync "" 1)
    (indexer/run-until-idle! ix)
    p))

(deftest indexes-a-project-its-jar-and-external-dirs
  (let [{:keys [root]} (fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [c (:c ix)
            p (sync-project! ix root)]
        (is (every? (visible-defs c p) ["app.a/fa" "app.b/fb" "acme.lib/from-jar" "ext.core/from-ext"
                                         ;; clojure itself is on the classpath
                                         "clojure.core/map"]))
        (is (not (contains? (visible-defs c p) "acme.lib/hidden")) "dependencies' private vars are dropped")
        (is (zero? (queue/pending-count c p)))

        (testing "an edited file is re-analyzed"
          (spit (io/file root "src/app/b.clj") "(ns app.b) (defn fb2 [] 1)")
          (queue/enqueue! c p :file (str root "/src/app/b.clj") 0)
          (indexer/run-until-idle! ix)
          (is (contains? (visible-defs c p) "app.b/fb2"))
          (is (not (contains? (visible-defs c p) "app.b/fb"))))

        (testing "a deleted file disappears"
          (io/delete-file (io/file root "src/app/b.clj"))
          (queue/enqueue! c p :delete (str root "/src/app/b.clj") 0)
          (indexer/run-until-idle! ix)
          (is (not (contains? (visible-defs c p) "app.b/fb2"))))

        (testing "a new file in a source dir is picked up when the editor asks"
          (spit (io/file root "src/app/c.clj") "(ns app.c) (defn fc [] 1)")
          (queue/enqueue! c p :file (str root "/src/app/c.clj") 0)
          (indexer/run-until-idle! ix)
          (is (contains? (visible-defs c p) "app.c/fc")))))))

(deftest a-second-worktree-reuses-everything
  (let [{:keys [root]} (fixture)
        root2 (project! {"deps.edn" (slurp (io/file root "deps.edn"))
                         "src/app/a.clj" (slurp (io/file root "src/app/a.clj"))
                         "src/app/b.clj" (slurp (io/file root "src/app/b.clj"))})]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [c (:c ix)
            p1 (sync-project! ix root)
            units (count-of c "unit")
            jars (count-of c "jar")
            p2 (sync-project! ix root2)]
        (is (= (visible-defs c p1) (visible-defs c p2)))
        (is (= units (count-of c "unit")) "no file analyzed again")
        (is (= jars (count-of c "jar")) "no jar analyzed again")))))

(deftest resync-after-no-change-analyzes-nothing
  (let [{:keys [root]} (fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [c (:c ix)
            p (sync-project! ix root)
            analyzed (atom 0)]
        (with-redefs [csl.analyze/analyze-files (fn [& _] (swap! analyzed inc) [])
                      csl.analyze/analyze-jars (fn [& _] (swap! analyzed inc) [])]
          (queue/enqueue! c p :sync "" 1)
          (indexer/run-until-idle! ix))
        (is (zero? @analyzed))))))

(deftest analysis-overlaps-writing
  ;; while one batch is written, the next one is already being analyzed
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a)" "src/app/b.clj" "(ns app.b)" "src/app/c.clj" "(ns app.c)"})]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir) :batch-sizes {:file 1}})]
      (let [c (:c ix)
            p (sync-project! ix root)
            events (atom [])
            now #(System/nanoTime)
            real-analyze csl.analyze/analyze-files
            real-write csl.writer/write-units!]
        (doseq [f ["a" "b" "c"]]
          (spit (io/file root (str "src/app/" f ".clj")) (str "(ns app." f ") (defn changed [] 1)"))
          (queue/enqueue! c p :file (str root "/src/app/" f ".clj") 1))
        (with-redefs [csl.analyze/analyze-files (fn [paths opts]
                                                  (swap! events conj [:analyze (.getName (io/file (first paths))) (now)])
                                                  (Thread/sleep 300)
                                                  (real-analyze paths opts))
                      csl.writer/write-units! (fn [w units]
                                                (let [r (real-write w units)]
                                                  (swap! events conj [:written (count units) (now)])
                                                  r))]
          (indexer/run-until-idle! ix))
        (let [at (fn [kind name] (some (fn [[k n t]] (when (and (= k kind) (= n name)) t)) @events))
              writes (map last (filter #(= :written (first %)) @events))]
          (is (< (at :analyze "b.clj") (first writes)) "b's analysis began before a was written")
          (is (contains? (visible-defs c p) "app.c/changed")))))))
