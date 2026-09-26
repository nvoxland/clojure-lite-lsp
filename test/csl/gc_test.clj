(ns csl.gc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.db :as db]
   [csl.gc :as gc]
   [csl.snapshot :as snapshot]
   [csl.test-util :as tu]
   [csl.writer :as writer]
   [csl.writer-test :refer [unit-key analyzed]]))

(defn with-writer [f]
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (f (writer/writer c) c)))

(defn count-of [c sql & params] (apply db/query-value c (str "SELECT count(*) FROM " sql) params))

(defn searchable? [c text]
  (some? (db/query-value c "SELECT rowid FROM name_fts WHERE rowid = (SELECT id FROM sym WHERE text = ?)" text)))

(defn file! [w p path code]
  (let [[u] (writer/write-units! w [[(unit-key code) (analyzed code "x.clj")]])]
    (snapshot/set-file-unit! w p path u {:ord 0})
    u))

(deftest units-no-project-sees-are-collected
  (with-writer
    (fn [w c]
      (let [p (snapshot/ensure-project! c "/a")
            kept (file! w p "/a/kept.clj" "(ns kept (:require [gone])) (defn shared [] 1) (defn only-kept \"doc\" [] (gone/g))")
            dead (file! w p "/a/gone.clj" "(ns gone) (defn shared [] 2) (defn only-gone \"doc\" [x] x)")]
        (snapshot/remove-file! w p "/a/gone.clj")
        (is (= {:units 1} (select-keys (gc/collect! w {}) [:units])))
        (testing "the dead unit and every row of it"
          (is (zero? (count-of c "unit WHERE id = ?" dead)))
          (doseq [t ["definition" "file_element" "usage"]]
            (is (zero? (count-of c (str t " WHERE unit_id = ?") dead)) t))
          (is (= 1 (count-of c "doc"))))
        (testing "the live unit is untouched"
          (is (pos? (count-of c "definition WHERE unit_id = ?" kept)))
          (is (pos? (count-of c "usage WHERE unit_id = ?" kept))))
        (testing "search entries for names nothing defines any more"
          (is (not (searchable? c "only-gone")))
          (is (searchable? c "shared"))
          (is (searchable? c "only-kept")))))))

(deftest units-another-project-sees-survive
  (with-writer
    (fn [w c]
      (let [p1 (snapshot/ensure-project! c "/wt1")
            p2 (snapshot/ensure-project! c "/wt2")
            code "(ns same) (defn f [] 1)"
            u (file! w p1 "/wt1/same.clj" code)]
        (file! w p2 "/wt2/same.clj" code)
        (snapshot/remove-file! w p1 "/wt1/same.clj")
        (gc/collect! w {})
        (is (= 1 (count-of c "unit WHERE id = ?" u)))))))

(deftest jars-no-project-uses-are-collected
  (with-writer
    (fn [w c]
      (let [p (snapshot/ensure-project! c "/a")
            j (snapshot/write-jar! w {:jar-hash (.getBytes "j") :config-hash (byte-array 1)
                                      :kondo-version "t" :options-hash (byte-array 1)}
                                   [["lib/core.clj" (unit-key "lib" :external? true) (analyzed "(ns lib.core) (defn lf [] 1)" "lib/core.clj")]
                                    ["lib/X.class" (unit-key "cls" :external? true) [{:kind :java-class-def :name "lib.X" :lang #{:clj}}]]])]
        (snapshot/set-project-jars! w p [[1 "/m2/lib.jar" j]])
        (gc/collect! w {})
        (is (= 1 (count-of c "jar")) "still on the classpath")
        (snapshot/set-project-jars! w p [])
        (is (= {:jars 1 :units 1} (select-keys (gc/collect! w {}) [:jars :units])))
        (doseq [t ["jar" "jar_entry" "java_class" "unit" "definition"]]
          (is (zero? (count-of c t)) t))))))

(deftest projects-not-seen-for-a-while-are-dropped
  (with-writer
    (fn [w c]
      (let [old (snapshot/ensure-project! c "/old")
            fresh (snapshot/ensure-project! c "/fresh")]
        (file! w old "/old/a.clj" "(ns old-a)")
        (file! w fresh "/fresh/a.clj" "(ns fresh-a)")
        (db/execute! c "UPDATE project SET last_seen = 0 WHERE id = ?" old)
        (is (= {:projects 1 :units 1} (select-keys (gc/collect! w {:project-max-age-ms (* 1000 60 60 24 30)})
                                                   [:projects :units])))
        (is (= ["/fresh"] (map first (db/query c "SELECT root FROM project"))))
        (is (zero? (count-of c "project_file WHERE project_id = ?" old)))))))

(deftest the-writer-keeps-working-after-collection
  (with-writer
    (fn [w c]
      (let [p (snapshot/ensure-project! c "/a")]
        (file! w p "/a/a.clj" "(ns a) (defn frobnicate [] 1)")
        (snapshot/remove-file! w p "/a/a.clj")
        (gc/collect! w {})
        (is (not (searchable? c "frobnicate")))
        (file! w p "/a/b.clj" "(ns b) (defn frobnicate [] 2)")
        (is (searchable? c "frobnicate") "the writer must not remember the collected entry")))))
