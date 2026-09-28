(ns clojure-lite-lsp.gc-test
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.gc :as gc]
   [clojure-lite-lsp.index-fixture :refer [count-of unit-key analyzed with-writer file!]]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.writer :as writer]
   [clojure.test :refer [deftest is testing]]))

(defn searchable? [c text]
  (some? (db/query-value c "SELECT rowid FROM name_fts WHERE rowid = (SELECT id FROM sym WHERE text = ?)" text)))

(deftest units-no-project-sees-are-collected
  (with-writer [w c]
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
        (is (searchable? c "only-kept"))))))

(deftest units-another-project-sees-survive
  (with-writer [w c]
    (let [p1 (snapshot/ensure-project! c "/wt1")
          p2 (snapshot/ensure-project! c "/wt2")
          code "(ns same) (defn f [] 1)"
          u (file! w p1 "/wt1/same.clj" code)]
      (file! w p2 "/wt2/same.clj" code)
      (snapshot/remove-file! w p1 "/wt1/same.clj")
      (gc/collect! w {})
      (is (= 1 (count-of c "unit WHERE id = ?" u))))))

(deftest jars-no-project-uses-are-collected
  (with-writer [w c]
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
        (is (zero? (count-of c t)) t)))))

(deftest projects-not-seen-for-a-while-are-dropped
  (with-writer [w c]
    (let [old (snapshot/ensure-project! c "/old")
          fresh (snapshot/ensure-project! c "/fresh")]
      (file! w old "/old/a.clj" "(ns old-a)")
      (file! w fresh "/fresh/a.clj" "(ns fresh-a)")
      (db/execute! c "UPDATE project SET last_seen = 0 WHERE id = ?" old)
      (is (= {:projects 1 :units 1} (select-keys (gc/collect! w {:project-max-age-ms gc/default-project-max-age-ms})
                                                 [:projects :units])))
      (is (= ["/fresh"] (map first (db/query c "SELECT root FROM project"))))
      (is (zero? (count-of c "project_file WHERE project_id = ?" old))))))

(deftest the-writer-keeps-working-after-collection
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")]
      (file! w p "/a/a.clj" "(ns a) (defn frobnicate [] 1)")
      (snapshot/remove-file! w p "/a/a.clj")
      (gc/collect! w {})
      (is (not (searchable? c "frobnicate")))
      (file! w p "/a/b.clj" "(ns b) (defn frobnicate [] 2)")
      (is (searchable? c "frobnicate") "the writer must not remember the collected entry"))))

(deftest opened-library-files-live-as-long-as-their-jar
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")
          hash-bytes (.getBytes "j")
          j (snapshot/write-jar! w {:jar-hash hash-bytes :config-hash (byte-array 1)
                                    :kondo-version "t" :options-hash (byte-array 1)}
                                 [["lib/core.clj" (unit-key "lib" :external? true) (analyzed "(ns lib.core) (defn lf [] 1)" "lib/core.clj")]])
          [u] (writer/write-units! w [[(unit-key "full lib" :external? true) (analyzed "(ns lib.core) (defn lf [] (inc 1))" "lib/core.clj")]])
          extracted (str (tu/temp-dir) "/sources/6a/lib/core.clj")]
      (snapshot/set-project-jars! w p [[1 "/m2/lib.jar" j]])
      (snapshot/set-dep-file-unit! w extracted hash-bytes u)
      (gc/collect! w {})
      (is (= 1 (count-of c "unit WHERE id = ?" u)) "not visible to any project, but opened")
      (snapshot/set-project-jars! w p [])
      (gc/collect! w {})
      (is (zero? (count-of c "dep_file")))
      (is (zero? (count-of c "unit"))))))

(defn sym? [c text] (some? (db/query-value c "SELECT id FROM sym WHERE text = ?" text)))

(deftest symbols-nothing-refers-to-are-swept
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")]
      (file! w p "/a/kept.clj" "(ns kept) (defn shared-name [] 1)")
      (file! w p "/a/gone.clj" "(ns gone) (defn shared-name [] 2) (defn only-in-gone [] (kept/shared-name))")
      (snapshot/remove-file! w p "/a/gone.clj")
      (gc/collect! w {})
      (is (not (sym? c "only-in-gone")))
      (is (not (sym? c "gone")))
      (is (sym? c "shared-name"))
      (is (sym? c "kept"))
      (testing "the writer interns a swept name again"
        (file! w p "/a/back.clj" "(ns back) (defn only-in-gone [] 3)")
        (is (sym? c "only-in-gone"))))))

(deftest fingerprints-of-files-no-project-has-are-pruned
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")]
      (doseq [path ["/a/kept.clj" "/a/gone.clj" "/elsewhere/x.clj"]]
        (db/execute! c "INSERT INTO fingerprint VALUES (?, 1, 1, X'00')" path))
      (file! w p "/a/kept.clj" "(ns kept)")
      (gc/collect! w {})
      (is (= [["/a/kept.clj"]] (db/query c "SELECT path FROM fingerprint"))))))

(deftest usages-at-one-spot-are-all-collected
  ;; two usages at the same position, kind and language, of different
  ;; vars (an unresolved-namespace finding beside an analysis usage): GC
  ;; finds a unit's usages through its file elements, so both need one
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")
          usage (fn [ns] {:kind :var-usage :ns ns :name "f" :lang #{:clj} :pos [1 1 1 5] :flags #{}})
          [u] (writer/write-units! w [[(unit-key "twins") [(usage "one") (usage "two")]]])]
      (snapshot/set-file-unit! w p "/a/x.clj" u {:ord 0})
      (is (= 2 (count-of c "usage WHERE unit_id = ?" u)))
      (snapshot/remove-file! w p "/a/x.clj")
      (gc/collect! w {})
      (is (zero? (count-of c "usage WHERE unit_id = ?" u))))))

(deftest ids-are-not-reused-after-collection
  ;; something still keyed by a collected id must not come to mean a new row
  (with-writer [w c]
    (let [p (snapshot/ensure-project! c "/a")
          gone (file! w p "/a/gone.clj" "(ns gone) (defn g [] 1)")]
      (snapshot/remove-file! w p "/a/gone.clj")
      (gc/collect! w {})
      (is (< gone (file! w p "/a/new.clj" "(ns fresh) (defn n [] 1)"))))))
