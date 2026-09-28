(ns clojure-lite-lsp.project-files-test
  "Files the classpath doesn't name, and projects whose classpath can't be
  computed, still get answers."
  (:require
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.indexer-test :refer [visible-defs sync-project!]]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.status :as status]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest a-file-outside-the-source-paths
  ;; build.clj in a tools.build project, scripts/: opened in an editor
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a) (defn f [] 1)"
                        "build.clj" "(ns build) (defn jar [_] 1)"})
        build (.getCanonicalPath (io/file root "build.clj"))]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [p (sync-project! ix root)]
        (is (not (contains? (visible-defs (:c ix) p) "build/jar")) "not indexed on its own")
        (queue/enqueue! (:c ix) p :file build 0)
        (indexer/run-until-idle! ix)
        (is (contains? (visible-defs (:c ix) p) "build/jar") "once opened, it is")
        (sync-project! ix root)
        (is (contains? (visible-defs (:c ix) p) "build/jar") "and stays through a sync")
        (io/delete-file (io/file build))
        (sync-project! ix root)
        (is (not (contains? (visible-defs (:c ix) p) "build/jar")) "until it's gone")))))

(deftest a-classpath-that-cant-be-computed-at-first
  ;; a broken deps.edn on first open, or the build tool missing from a GUI
  ;; editor's PATH: src/ and test/ meanwhile, and the error kept
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [root (project! {"deps.edn" "{:paths [\"src\"" "src/app/a.clj" "(ns app.a)" "test/app/a_test.clj" "(ns app.a-test)"})
          p (snapshot/ensure-project! c root)
          entries (classpath/memoized! c p root {:run (fn [_ _] (throw (ex-info "boom" {})))})]
      (is (= #{(str root "/src") (str root "/test")} (set (map :path (filter #(= :source-dir (:kind %)) entries)))))
      (is (some? (classpath/error c p)))))
  (testing "indexed that way, with the error in the status"
    (let [root (project! {"deps.edn" "{:paths [\"src\"" "src/app/a.clj" "(ns app.a) (defn f [] 1)"})]
      (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
        (let [p (sync-project! ix root)]
          (is (contains? (visible-defs (:c ix) p) "app.a/f"))
          (is (:classpath-error (first (:projects (status/data (:c ix)))))))))))

(deftest a-deleted-directory
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/old/x.clj" "(ns app.old.x) (defn x [] 1)"
                        "src/app/keep.clj" "(ns app.keep) (defn k [] 1)"})]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [p (sync-project! ix root)
            dir (.getCanonicalPath (io/file root "src/app/old"))]
        (doseq [f (reverse (file-seq (io/file dir)))] (.delete ^java.io.File f))
        (queue/enqueue! (:c ix) p :delete dir 1)
        (indexer/run-until-idle! ix)
        (is (not (contains? (visible-defs (:c ix) p) "app.old.x/x")))
        (is (contains? (visible-defs (:c ix) p) "app.keep/k"))))))
