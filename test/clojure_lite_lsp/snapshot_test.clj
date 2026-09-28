(ns clojure-lite-lsp.snapshot-test
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.index-fixture :refer [unit-key with-writer]]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.writer :as writer]
   [clojure.test :refer [deftest is testing]]))

(defn visible
  "project_unit for project `p`, as {unit-id ord}."
  [c p]
  (into {} (db/query c "SELECT unit_id, ord FROM project_unit WHERE project_id = ?" p)))

(defn unit! [w content]
  (first (writer/write-units! w [[(unit-key content) []]])))

(deftest projects-are-registered-once-per-root
  (with-writer [_ c]
    (let [p (snapshot/ensure-project! c "/src/a")]
      (is (= p (snapshot/ensure-project! c "/src/a")))
      (is (not= p (snapshot/ensure-project! c "/src/b"))))))

(deftest project-files-map-to-units
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/src/a")
          u1 (unit! w "v1")
          u2 (unit! w "v2")]
      (snapshot/set-file-unit! w p "/src/a/x.clj" u1 {:ord 0})
      (is (= {u1 0} (visible c p)))
      (testing "repointing a file drops the old unit"
        (snapshot/set-file-unit! w p "/src/a/x.clj" u2 {:ord 0})
        (is (= {u2 0} (visible c p))))
      (testing "a unit shared by two files stays while either uses it"
        (snapshot/set-file-unit! w p "/src/a/y.clj" u2 {:ord 0})
        (snapshot/set-file-unit! w p "/src/a/x.clj" u1 {:ord 0})
        (is (= {u1 0 u2 0} (visible c p))))
      (testing "removing a file"
        (snapshot/remove-file! w p "/src/a/y.clj")
        (is (= {u1 0} (visible c p)))))))

(deftest files-not-yet-indexed-are-listed-without-a-unit
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/src/a")]
      (snapshot/set-file-unit! w p "/src/a/x.clj" nil {:ord 0})
      (is (= [["/src/a/x.clj" nil]] (db/query c "SELECT path, unit_id FROM project_file")))
      (is (= {} (visible c p))))))

(defn write-jar-rows! [w content entries]
  (snapshot/write-jar! w {:jar-hash (.getBytes (str content)) :config-hash (byte-array 1)
                          :kondo-version "test" :options-hash (byte-array 1)}
                       (for [[path unit-content] entries]
                         [path (unit-key unit-content :external? true) []])))

(deftest jars-are-content-addressed
  (with-writer [w c]
    (let [j1 (write-jar-rows! w "jar1" [["a/core.clj" "a"] ["a/util.clj" "b"]])]
      (is (= j1 (write-jar-rows! w "jar1" [["a/core.clj" "a"] ["a/util.clj" "b"]])))
      (is (= 2 (db/query-value c "SELECT count(*) FROM jar_entry WHERE jar_id = ?" j1)))
      (is (= 2 (db/query-value c "SELECT count(*) FROM unit"))))))

(deftest jar-java-classes-are-recorded-per-jar
  ;; in a large project, most definitions can be Java classes (58% in one),
  ;; one per .class file: they get no unit (nor unit, jar_entry and
  ;; project_unit rows each).
  (with-writer [w c]
    (let [j (snapshot/write-jar! w {:jar-hash (.getBytes "jar") :config-hash (byte-array 1)
                                    :kondo-version "test" :options-hash (byte-array 1)}
                                 [["java/io/File.class" (unit-key "cls" :external? true)
                                   [{:kind :java-class-def :name "java.io.File" :lang #{:clj}}]]
                                  ["a/core.clj" (unit-key "clj" :external? true)
                                   [{:kind :var-def :ns "a.core" :name "f" :lang #{:clj} :pos [1 1 1 2] :flags #{}}]]])
          p (snapshot/ensure-project! c "/src/a")]
      (is (= 1 (db/query-value c "SELECT count(*) FROM unit")))
      (is (= [["a/core.clj"]] (db/query c "SELECT entry_path FROM jar_entry")))
      (is (= [["java.io.File" j "java/io/File.class"]]
             (db/query c "SELECT s.text, jc.jar_id, jc.entry_path FROM java_class jc JOIN sym s ON s.id = jc.name")))
      (snapshot/set-project-jars! w p [[1 "/m2/x.jar" j]])
      (is (= 1 (count (visible c p)))))))

(deftest classpath-order-is-precedence
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/src/a")
          j1 (write-jar-rows! w "jar1" [["a/core.clj" "a"] ["shared.clj" "same"]])
          j2 (write-jar-rows! w "jar2" [["b/core.clj" "b"] ["shared.clj" "same"]])
          unit-of (fn [j path] (db/query-value c "SELECT unit_id FROM jar_entry WHERE jar_id = ? AND entry_path = ?" j path))]
                ;; [classpath-position path jar-id]; project sources are position 0
      (snapshot/set-project-jars! w p [[1 "/m2/jar2.jar" j2] [2 "/m2/jar1.jar" j1] [3 "/m2/pending.jar" nil]])
      (is (= {(unit-of j2 "b/core.clj") 1
              (unit-of j1 "a/core.clj") 2
                ;; identical content in both jars: the earlier classpath entry wins
              (unit-of j1 "shared.clj") 1}
             (visible c p)))
      (is (= [[1 "/m2/jar2.jar" j2] [2 "/m2/jar1.jar" j1] [3 "/m2/pending.jar" nil]]
             (db/query c "SELECT ord, path, jar_id FROM project_jar WHERE project_id = ? ORDER BY ord" p)))
      (testing "a new classpath replaces the old one"
        (snapshot/set-project-jars! w p [[1 "/m2/jar1.jar" j1]])
        (is (= {(unit-of j1 "a/core.clj") 1 (unit-of j1 "shared.clj") 1} (visible c p))))
      (testing "project files keep their own mappings"
        (let [u (unit! w "src")]
          (snapshot/set-file-unit! w p "/src/a/x.clj" u {:ord 0})
          (snapshot/set-project-jars! w p [])
          (is (= {u 0} (visible c p))))))))

(deftest a-jar-indexed-later-is-linked-incrementally
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/src/a")
          j1 (write-jar-rows! w "jar1" [["a.clj" "a"] ["shared.clj" "same"]])]
      (snapshot/set-project-jars! w p [[1 "/m2/jar1.jar" j1] [2 "/m2/jar2.jar" nil]])
      (let [before (visible c p)
            j2 (write-jar-rows! w "jar2" [["b.clj" "b"] ["shared.clj" "same"]])
            b (db/query-value c "SELECT unit_id FROM jar_entry WHERE jar_id = ? AND entry_path = 'b.clj'" j2)]
        (snapshot/link-jar! w p 2 j2)
        (is (= (assoc before b 2) (visible c p)) "the shared unit keeps the earlier position")
        (is (= j2 (db/query-value c "SELECT jar_id FROM project_jar WHERE project_id = ? AND ord = 2" p)))))))

(deftest projects-share-units-but-not-visibility
  (with-writer [w c]
    (let [p1 (snapshot/ensure-project! c "/wt/one")
          p2 (snapshot/ensure-project! c "/wt/two")
          u (unit! w "same content")]
      (snapshot/set-file-unit! w p1 "/wt/one/x.clj" u {:ord 0})
      (snapshot/set-file-unit! w p2 "/wt/two/x.clj" u {:ord 0})
      (snapshot/remove-file! w p1 "/wt/one/x.clj")
      (is (= {} (visible c p1)))
      (is (= {u 0} (visible c p2))))))
