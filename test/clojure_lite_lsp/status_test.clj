(ns clojure-lite-lsp.status-test
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.index-fixture :refer [project-using sync-project! temp-indexer]]
   [clojure-lite-lsp.status :as status]
   [clojure-lite-lsp.test-util :as tu :refer [jar!]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(deftest reports-what-projects-share
  (let [shared (jar! {"shared/lib.clj" "(ns shared.lib)"})
        a (project-using [shared (jar! {"a/lib.clj" "(ns a.lib)"})])
        b (project-using [shared])]
    (with-open [ix (temp-indexer)]
      (sync-project! ix a)
      (sync-project! ix b)
      (let [{:keys [projects index]} (status/data (:c ix))
            by-root (into {} (map (juxt :root identity)) projects)
            pa (by-root a) pb (by-root b)]
        (testing "per project: files, jars, and the jars also used by another project"
          (is (= {:files 1 :files-indexed 1 :pending 0} (select-keys pa [:files :files-indexed :pending])))
          ;; clojure, spec.alpha and core.specs.alpha, plus the shared jar
          (is (= 4 (:jars-shared pb)))
          (is (= (inc (:jars-shared pa)) (:jars pa)) "a's own jar is the only one not shared"))
        (testing "index totals"
          (is (= (count (distinct (concat (:jar-ids pa) (:jar-ids pb)))) (:jars index)))
          (is (pos? (:units index))))
        (testing "printed"
          (let [out (with-out-str (status/print! (status/data (:c ix))))]
            (is (str/includes? out a))
            (is (re-find #"jars: 5 \(4 shared with other projects\)" out))))
        (testing "with where the index is"
          (let [out (with-out-str (status/print! (status/data (:c ix)) {:path "/x/v1/index.db" :file-mb 3}))]
            (is (re-find #"Index: +3 MB, .*\n +/x/v1/index.db\n" out))))))))

(deftest a-daemon-that-died-isnt-reported-running
  ;; its row outlives a crash; its lock doesn't
  (with-open [ix (temp-indexer)]
    (db/execute! (:c ix) "INSERT INTO daemon (id, pid, version, started_at, heartbeat_at, stop_requested)
                              VALUES (1, 4242, 'x', 0, 0, 0)")
    (is (nil? (:daemon (status/data (:c ix) {:daemon-alive? false}))))
    (is (= 4242 (get-in (status/data (:c ix) {:daemon-alive? true}) [:daemon :pid])))))
