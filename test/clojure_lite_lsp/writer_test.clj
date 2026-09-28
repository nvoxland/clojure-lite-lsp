(ns clojure-lite-lsp.writer-test
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.index-fixture :refer [unit-key analyzed with-writer]]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.writer :as writer]
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]))

(defn sym [c id] (db/query-value c "SELECT text FROM sym WHERE id = ?" id))

(deftest writes-each-access-pattern-table
  (with-writer [w c]
    (let [code "(ns a (:require [clojure.string :as str]))
(defn f \"Doc of f.\" [x] (let [y x] (str/join y)))"
          [u] (writer/write-units! w [[(unit-key code) (analyzed code "a.clj")]])]
      (testing "definition by (ns, name), with its doc"
        (let [[[id kind lang]] (db/query c "SELECT d.id, d.kind, d.lang FROM definition d
                                              JOIN sym n ON n.id = d.ns JOIN sym m ON m.id = d.name
                                              WHERE n.text = 'a' AND m.text = 'f'")]
          (is (= (kinds/code :var-def) kind))
          (is (= 1 lang))
          (is (= "Doc of f." (db/query-value c "SELECT docstring FROM doc WHERE definition_id = ?" id)))))
      (testing "usage by target, with the calling var"
        (is (= [["a" "f"]]
               (map (fn [[fns fv]] [(sym c fns) (sym c fv)])
                    (db/query c "SELECT u.from_ns, u.from_var FROM usage u
                                   JOIN sym n ON n.id = u.to_ns JOIN sym m ON m.id = u.name
                                   WHERE n.text = 'clojure.string' AND m.text = 'join' AND u.unit_id = ?" u)))))
      (testing "file elements by position, including locals and the alias"
        (let [kinds-at (fn [row] (set (map #(kinds/kind (first %))
                                           (db/query c "SELECT kind FROM file_element WHERE unit_id = ? AND name_row = ?" u row))))]
          (is (set/subset? #{:ns-def :ns-usage :ns-alias} (kinds-at 1)))
          (is (set/subset? #{:var-def :local :local-usage :var-usage} (kinds-at 2))))))))

(deftest units-are-content-addressed
  (with-writer [w c]
    (let [code "(ns a) (defn f [] 1)"
          [u1] (writer/write-units! w [[(unit-key code) (analyzed code "a.clj")]])
          [u2] (writer/write-units! w [[(unit-key code) (analyzed code "a.clj")]])]
      (is (= u1 u2))
      (is (= 1 (db/query-value c "SELECT count(*) FROM definition WHERE kind = ?" (kinds/code :var-def)))))))

(deftest duplicate-elements-are-stored-once
  (with-writer [w c]
    (let [el {:kind :var-usage :ns "b" :name "g" :lang #{:clj} :pos [1 1 1 2] :flags #{}}]
      (writer/write-units! w [[(unit-key "dup") [el el]]])
      (is (= 1 (db/query-value c "SELECT count(*) FROM usage")))
      (is (= 1 (db/query-value c "SELECT count(*) FROM file_element"))))))

(deftest symbols-and-ids-survive-a-new-writer
  (let [path (tu/temp-db-path)]
    (with-open [c (db/open-writer path)]
      (writer/write-units! (writer/writer c) [[(unit-key 1) (analyzed "(ns a) (defn f [] 1)" "a.clj")]]))
    (with-open [c (db/open-writer path)]
      (writer/write-units! (writer/writer c) [[(unit-key 2) (analyzed "(ns b) (defn f [] 1)" "b.clj")]])
      (testing "names are interned once"
        (is (= 1 (db/query-value c "SELECT count(*) FROM sym WHERE text = 'f'"))))
      (is (= 2 (db/query-value c "SELECT count(*) FROM unit")))
      (is (= 2 (db/query-value c "SELECT count(*) FROM definition WHERE kind = ?" (kinds/code :var-def)))))))

(deftest definition-names-are-searchable
  (with-writer [w c]
    (writer/write-units! w [[(unit-key 1) (analyzed "(ns a) (defn frobnicate [] 1)" "a.clj")]
                            [(unit-key 2) (analyzed "(ns b) (defn frobnicate [] 2)" "b.clj")]])
    (is (= ["frobnicate"]
           (map #(sym c (first %)) (db/query c "SELECT rowid FROM name_fts WHERE name_fts MATCH '\"obnic\"'"))))))

(deftest a-failed-chunk-writes-nothing
  (with-writer [w c]
    (is (thrown? Exception
                 (writer/write-units! w [[(unit-key 1) (analyzed "(ns a) (defn f [] 1)" "a.clj")]
                                         [(unit-key 2) [{:kind :no-such-kind :name "x" :lang #{:clj} :pos [1 1 1 2]}]]])))
    (is (zero? (db/query-value c "SELECT count(*) FROM unit")))
    (is (zero? (db/query-value c "SELECT count(*) FROM definition")))
    (testing "and the writer still works afterwards"
      (writer/write-units! w [[(unit-key 3) (analyzed "(ns c) (defn h [] 1)" "c.clj")]])
      (is (= 1 (db/query-value c "SELECT count(*) FROM unit"))))))

(deftest refs-are-recorded-per-unit
  (with-writer [w c]
    (let [code "(ns a (:require [clojure.string :as str])) (defn f [] (str/join []))"
          [u] (writer/write-units! w [[(unit-key code) (analyzed code "a.clj")]])]
      (is (set/subset? #{"clojure.string/join" "clojure.core/defn" "ns:a"} (set (writer/unit-refs c u))))
      (is (zero? (db/query-value c "SELECT count(*) FROM file_element WHERE kind = 0"))))))

(deftest units-are-found-by-alias-and-by-base-key
  (with-writer [w c]
    (let [k1 (unit-key "same content" :config-hash (byte-array [1]))
          k2 (unit-key "same content" :config-hash (byte-array [2]))
          [u] (writer/write-units! w [[k1 (analyzed "(ns a)" "a.clj")]])]
      (testing "another config: not the same key, but the same base"
        (is (nil? (writer/unit-id c k2)))
        (is (= [[u [1]]] (map (fn [[id h]] [id (vec h)]) (writer/units-with-base c k2)))))
      (testing "an alias makes the other config's key find the unit"
        (writer/add-unit-key! w k2 u)
        (is (= u (writer/unit-id c k2)))
        (is (= u (writer/unit-id c k1)))))))
