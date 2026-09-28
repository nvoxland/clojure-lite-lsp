(ns clojure-lite-lsp.sharing-test
  "Analysis shared across different projects: a jar is analyzed once, ever,
  unless the projects give it genuinely different configs."
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.gc :as gc]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.indexer-test :refer [visible-defs sync-project! count-of]]
   [clojure-lite-lsp.kondo-config-test :refer [jar! maven-jar!]]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(defn project-using
  "A project whose deps.edn uses the given jars, with one source file. A
  jar is a path, or [lib path] for jars with Maven metadata: tools.deps
  reads the poms inside local jars, so a jar depending on acme/y resolves
  only when acme/y is itself declared under that name."
  [jars]
  (project! {"deps.edn" (pr-str {:paths ["src"]
                                 :deps (into {} (map-indexed (fn [i j]
                                                               (if (vector? j)
                                                                 [(first j) {:local/root (second j)}]
                                                                 [(symbol "dep" (str "d" i)) {:local/root j}]))
                                                             jars))})
             "src/app/core.clj" (str "(ns app.core) (defn own-" (hash jars) " [] 1)")}))

(defn analyzed-jars
  "Run f, returning the file names of the jars analyzed meanwhile."
  [f]
  (let [seen (atom [])
        real analyze/analyze-jars]
    (with-redefs [analyze/analyze-jars (fn [jars opts]
                                         (swap! seen into (map #(.getName (io/file ^String %)) jars))
                                         (real jars opts))]
      (f))
    @seen))

(deftest different-projects-share-identical-jars
  (let [shared (jar! {"shared/lib.clj" "(ns shared.lib) (defn from-shared [] 1)"})
        only-a (jar! {"a/lib.clj" "(ns a.lib) (defn from-a [] 1)"})
        only-b (jar! {"b/lib.clj" "(ns b.lib) (defn from-b [] 1)"})
        a (project-using [shared only-a])
        b (project-using [shared only-b])]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [c (:c ix)
            pa (sync-project! ix a)
            before (count-of c "jar")
            analyzed-for-b (analyzed-jars #(sync-project! ix b))
            pb (db/query-value c "SELECT id FROM project WHERE root = ?" b)]
        (testing "the second project analyzes only the jar it doesn't share"
          (is (= 1 (count analyzed-for-b)))
          (is (= (inc before) (count-of c "jar"))))
        (is (every? (visible-defs c pa) ["shared.lib/from-shared" "a.lib/from-a"]))
        (is (every? (visible-defs c pb) ["shared.lib/from-shared" "b.lib/from-b"]))
        (is (not (contains? (visible-defs c pb) "a.lib/from-a")))))))

(def exports-v1 "{:lint-as {acme.y/defthing clojure.core/def}}")
(def exports-v3 "{:lint-as {acme.y/defthing clojure.core/declare}}")

(defn y-jar
  "acme/y: exports a clj-kondo config teaching its defthing macro."
  [version exports]
  (maven-jar! "acme" "y" [] {"acme/y.clj" (str "(ns acme.y) ;; " version "\n(defmacro defthing [n v] `(def ~n ~v))")
                             "clj-kondo.exports/acme/y/config.edn" exports}))

(def x-jar-source "(ns acme.x (:require [acme.y :refer [defthing]]))\n(defthing thing 1)")

(deftest a-dependency-at-another-version-with-the-same-exports-still-shares
  ;; the config hash is over the exports' content, not the version
  (let [x (maven-jar! "acme" "x" [["acme" "y"]] {"acme/x.clj" x-jar-source})
        p1 (project-using [['acme/x x] ['acme/y (y-jar "1.0" exports-v1)]])
        p2 (project-using [['acme/x x] ['acme/y (y-jar "2.0" exports-v1)]])]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (sync-project! ix p1)
      (let [analyzed (analyzed-jars #(sync-project! ix p2))]
        (is (= 1 (count analyzed)) "only the new version of y itself")))))

(deftest a-dependency-with-different-exports-gets-its-own-analysis
  ;; x's analysis depends on y's hooks and lint-as, so it can't be shared
  (let [x (maven-jar! "acme" "x" [["acme" "y"]] {"acme/x.clj" x-jar-source})
        p1 (project-using [['acme/x x] ['acme/y (y-jar "1.0" exports-v1)]])
        p3 (project-using [['acme/x x] ['acme/y (y-jar "3.0" exports-v3)]])]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [c (:c ix)
            pid1 (sync-project! ix p1)
            analyzed (analyzed-jars #(sync-project! ix p3))
            pid3 (db/query-value c "SELECT id FROM project WHERE root = ?" p3)]
        (is (= 2 (count analyzed)) "x again, and the new y")
        (testing "each project sees x as analyzed with its own y's config"
          (is (contains? (visible-defs c pid1) "acme.x/thing")))
        (testing "a variant lives while a project uses it"
          (db/execute! c "UPDATE project SET last_seen = 0 WHERE id = ?" pid3)
          ;; p3 was last seen in 1970; p1 just now (a short max age would
          ;; expire p1 too, as syncing takes more than a moment)
          (gc/collect! (:w ix) {:project-max-age-ms (* 1000 60 60)})
          (is (contains? (visible-defs c pid1) "acme.x/thing"))
          (is (= (db/query-value c "SELECT count(*) FROM project_jar WHERE project_id = ?" pid1) (count-of c "jar"))
              "only p1's jars (x, y 1.0, and Clojure's own) remain"))))))
