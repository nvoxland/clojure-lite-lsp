(ns csl.status-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [csl.indexer :as indexer]
   [csl.indexer-test :refer [sync-project!]]
   [csl.kondo-config-test :refer [jar!]]
   [csl.sharing-test :refer [project-using]]
   [csl.status :as status]
   [csl.test-util :as tu]))

(deftest reports-what-projects-share
  (let [shared (jar! {"shared/lib.clj" "(ns shared.lib)"})
        a (project-using [shared (jar! {"a/lib.clj" "(ns a.lib)"})])
        b (project-using [shared])]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
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
            (is (re-find #"jars: 5 \(4 shared with other projects\)" out))))))))
