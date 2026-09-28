(ns clojure-lite-lsp.query-references-test
  (:require
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.query-fixture :as qf :refer [implementations locs references with-project]]
   [clojure.test :refer [deftest is testing]]))

(deftest every-way-of-reaching-a-var-is-a-reference
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [] 1)\n(defn g [] (f))"
                       "src/b.clj" "(ns b (:require [a :as al] [a :refer [f]]))\n(al/f) (f) (a/f) 'a/f"}]
    ;; a usage's position is the whole symbol as written: al/f starts at "al"
    (let [usages (locs proj ["src/a.clj" "f))"] ["src/b.clj" "f]"] ["src/b.clj" "al/f"] ["src/b.clj" "f) (a"]
                       ["src/b.clj" "a/f" 0] ["src/b.clj" "a/f" 1])]
      (testing "every way of reaching the var, from its definition or any usage"
        (is (= usages (references proj "src/a.clj" "f [")))
        (is (= usages (references proj "src/b.clj" "al/f"))))
      (testing "with the declaration"
        (is (= (conj usages ["src/a.clj" (qf/at proj "src/a.clj" "f [")])
               (references proj "src/b.clj" "al/f" :include-declaration? true)))))))

(deftest recursion-counts-only-with-the-declaration
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [n] (when (pos? n) (f (dec n))))\n(f 3)"}]
    (is (= (locs proj ["src/a.clj" "f 3"]) (references proj "src/a.clj" "f [")))
    (is (= (locs proj ["src/a.clj" "f 3"] ["src/a.clj" "f (dec"] ["src/a.clj" "f ["])
           (references proj "src/a.clj" "f [" :include-declaration? true)))))

(deftest records-include-their-constructors
  (with-project [proj {"src/a.clj" "(ns a)\n(defrecord R [x])\n(->R 1) (map->R {}) (R. 1)"}]
    (is (every? (references proj "src/a.clj" "R [")
                (locs proj ["src/a.clj" "->R"] ["src/a.clj" "map->R"])))))

(deftest a-keywords-uses-are-its-references
  (with-project [proj {"src/a.clj" "(ns a (:require [re-frame.core :as rf]))\n(rf/reg-event-db ::save identity)\n(::save {})"
                       "src/b.clj" "(ns b (:require [a]))\n(:a/save {}) (:other {})"}]
    (is (= (locs proj ["src/a.clj" "::save" 1] ["src/b.clj" ":a/save"])
           (references proj "src/b.clj" ":a/save")))))

(deftest a-locals-uses-are-its-references
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [x] (let [x2 x] (+ x x2)))"}]
    (is (= (locs proj ["src/a.clj" "x]" 1] ["src/a.clj" "x x2"])
           (references proj "src/a.clj" "x]")))))

(deftest a-namespaces-requires-are-its-references
  (with-project [proj {"src/a.clj" "(ns a)"
                       "src/b.clj" "(ns b (:require [a :as al]))"
                       "src/c.clj" "(ns c (:require [a]))"}]
    (is (= (locs proj ["src/b.clj" "a :as"] ["src/b.clj" "al]"] ["src/c.clj" "a]"])
           (references proj "src/a.clj" "a)")))))

(deftest an-aliases-uses-are-in-its-file
  ;; an alias is the file's: its uses there, not the namespace's references
  (with-project [proj {"src/a.clj" "(ns a) (defn f [] 1) (defn g [] 2)"
                       "src/b.clj" "(ns b (:require [a :as al]))\n(al/f) (al/g) ::al/k"
                       "src/c.clj" "(ns c (:require [a :as al]))\n(al/f)"}]
    (is (= (locs proj ["src/b.clj" "al/f"] ["src/b.clj" "al/g"] ["src/b.clj" "::al/k"])
           (references proj "src/b.clj" "al]")))
    (testing "with the declaration: the alias too"
      (is (= (locs proj ["src/b.clj" "al]"] ["src/b.clj" "al/f"] ["src/b.clj" "al/g"] ["src/b.clj" "::al/k"])
             (references proj "src/b.clj" "al]" :include-declaration? true))))))

(deftest a-protocols-implementations
  (with-project [proj {"src/a.clj" "(ns a)\n(defprotocol P (m [this]))\n(defrecord R [] P (m [this] 1))\n(extend-protocol P String (m [s] 2))\n(m (->R))"}]
    (let [impls (locs proj ["src/a.clj" "m [this] 1"] ["src/a.clj" "m [s]"])]
      (is (= impls (implementations proj "src/a.clj" "m [this]")))
      (testing "from a call"
        (is (= impls (implementations proj "src/a.clj" "m (->R")))))))

(deftest a-multimethods-methods-are-its-implementations
  (with-project [proj {"src/a.clj" "(ns a)\n(defmulti mm :type)\n(defmethod mm :x [_] 1)\n(defmethod mm :y [_] 2)\n(mm {})"}]
    (is (= (locs proj ["src/a.clj" "mm :x"] ["src/a.clj" "mm :y"])
           (implementations proj "src/a.clj" "mm :type")))))

(deftest java-class-references
  ;; who uses a class, across the project's Clojure code
  (with-project [proj {"src/a.clj" "(ns a (:import [java.io File]))\n(defn f [] (File. \"x\"))"
                       "src/b.clj" "(ns b)\n(defn g [] (java.io.File. \"y\") (str \"z\"))"}]
    (is (= (locs proj ["src/a.clj" "File]"] ["src/a.clj" "File. \"x"] ["src/b.clj" "java.io.File."])
           (references proj "src/a.clj" "File. \"x")) "the import counts too")))

(deftest java-class-jars-include-nested-classes
  ;; clj-kondo records every .class file, nested ones (Outer$Inner) included
  (with-project [proj {"src/a.clj" "(ns a)"}]
    (is (seq (q/java-class-jars (:c proj) (:p proj) "clojure.lang.Compiler")))
    (is (= (q/java-class-jars (:c proj) (:p proj) "clojure.lang.Compiler")
           (q/java-class-jars (:c proj) (:p proj) "clojure.lang.Compiler$C")))))
