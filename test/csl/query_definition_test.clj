(ns csl.query-definition-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.query :as q]
   [csl.query-fixture :as f :refer [with-project]]))

(defn definition
  "Go to definition from the `n`th occurrence of `needle` in `file`."
  [proj file needle & [n]]
  (let [[row col] (f/at proj file needle (or n 0))]
    (mapv #(f/rel proj %) (q/definition (:c proj) (:p proj) (f/path proj file) row col))))

(defn loc
  "Where the `n`th occurrence of `needle` in `file` starts, as a result."
  [proj file needle & [n]]
  [[file (f/at proj file needle (or n 0))]])

(deftest element-at-cursor
  (with-project [proj {"src/a.clj" "(ns a (:require [clojure.string :as str]))\n(defn f [x] (str/join x))"}]
    (let [[row col] (f/at proj "src/a.clj" "join")]
      (is (= [{:kind :var-usage :ns "clojure.string" :name "join" :alias "str"}]
             (map #(select-keys % [:kind :ns :name :alias])
                  (q/elements-at (:c proj) (:p proj) (f/path proj "src/a.clj") row (+ col 2))))))
    (testing "nothing between elements"
      (let [[row col] (f/at proj "src/a.clj" " (str/join")]
        (is (empty? (q/elements-at (:c proj) (:p proj) (f/path proj "src/a.clj") row col)))))))

(deftest var-definitions
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [] 1)\n(defn g [] (f))"
                       "src/b.clj" "(ns b (:require [a :as al] [a :refer [f]]))\n(al/f) (f) (a/g)"}]
    (testing "in the same namespace"
      (is (= (loc proj "src/a.clj" "f [") (definition proj "src/a.clj" "f)"))))
    (testing "through an alias, a refer, or fully qualified"
      (is (= (loc proj "src/a.clj" "f [") (definition proj "src/b.clj" "al/f")))
      (is (= (loc proj "src/a.clj" "f [") (definition proj "src/b.clj" "f) (a")))
      (is (= (loc proj "src/a.clj" "g [") (definition proj "src/b.clj" "a/g"))))
    (testing "into a jar"
      (is (= ["clojure/core.clj"] (map first (definition proj "src/a.clj" "defn")))))))

(deftest namespaces-and-aliases
  (with-project [proj {"src/a.clj" "(ns a)"
                       "src/b.clj" "(ns b (:require [a :as al]))\n(al/nope)"}]
    (is (= (loc proj "src/a.clj" "a)") (definition proj "src/b.clj" "a :as")))
    (is (= (loc proj "src/a.clj" "a)") (definition proj "src/b.clj" "al]")))
    (testing "an alias with no such var goes to the namespace"
      (is (= (loc proj "src/a.clj" "a)") (definition proj "src/b.clj" "al/nope"))))))

(deftest locals
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [x] (let [y x] (+ x y)))"}]
    (is (= (loc proj "src/a.clj" "x]") (definition proj "src/a.clj" "x y")))
    (is (= (loc proj "src/a.clj" "y x") (definition proj "src/a.clj" "y)")))))

(deftest keywords
  (with-project [proj {"src/a.clj" "(ns a (:require [re-frame.core :as rf]))\n(rf/reg-event-db ::save identity)\n(::save {}) (:plain {})"}]
    (is (= (loc proj "src/a.clj" "::save") (definition proj "src/a.clj" "::save" 1)))
    (testing "an unregistered keyword is its own definition"
      (is (= (loc proj "src/a.clj" ":plain") (definition proj "src/a.clj" ":plain"))))))

(deftest declare-goes-to-the-real-definition
  (with-project [proj {"src/a.clj" "(ns a)\n(declare f)\n(defn g [] (f))\n(defn f [] 1)"}]
    (is (= (loc proj "src/a.clj" "f [] 1") (definition proj "src/a.clj" "f))")))))

(deftest protocol-impls-go-to-the-protocol-method
  (with-project [proj {"src/a.clj" "(ns a)\n(defprotocol P (m [this]))\n(defrecord R [] P (m [this] 1))"}]
    (is (= (loc proj "src/a.clj" "m [this]") (definition proj "src/a.clj" "m [this] 1")))))

(deftest cljc-files-match-languages
  (with-project [proj {"src/a.cljc" "(ns a)\n#?(:clj (defn f [] :clj) :cljs (defn f [] :cljs))"
                       "src/b.cljs" "(ns b (:require [a]))\n(a/f)"}]
    (is (= (loc proj "src/a.cljc" "f [] :cljs") (definition proj "src/b.cljs" "a/f")))))

(deftest cljs-macros-come-from-clj
  (with-project [proj {"src/m.clj" "(ns m)\n(defmacro mac [x] x)"
                       "src/m.cljs" "(ns m (:require-macros [m]))"
                       "src/b.cljs" "(ns b (:require [m :include-macros true]))\n(m/mac 1)"}]
    (is (= (loc proj "src/m.clj" "mac [") (definition proj "src/b.cljs" "m/mac")))))

(deftest cljs-never-resolves-to-clj-functions
  ;; only macros cross from clj to cljs (:require-macros); a function
  ;; defined only for clj does not exist in cljs
  (with-project [proj {"src/m.clj" "(ns m)\n(defn only-clj [] 1)"
                       "src/b.cljs" "(ns b (:require [m]))\n(m/only-clj)"}]
    (is (= [] (definition proj "src/b.cljs" "m/only-clj")))))

(deftest clj-falls-back-to-cljs
  (with-project [proj {"src/m.cljs" "(ns m)\n(defn only-cljs [] 1)"
                       "src/b.clj" "(ns b (:require [m]))\n(m/only-cljs)"}]
    (is (= (loc proj "src/m.cljs" "only-cljs") (definition proj "src/b.clj" "m/only-cljs")))))

(deftest imported-vars-go-to-the-original
  (with-project [proj {"src/impl.clj" "(ns impl)\n(defn f [] 1)"
                       "src/api.clj" "(ns api (:require [potemkin :refer [import-vars]] [impl]))\n(import-vars [impl f])"
                       "src/b.clj" "(ns b (:require [api]))\n(api/f)"}]
    (is (= (loc proj "src/impl.clj" "f [") (definition proj "src/b.clj" "api/f")))))

(deftest java-classes-are-left-to-the-server
  ;; the query layer only names the class; finding its source is file work
  (with-project [proj {"src/a.clj" "(ns a (:import [java.io File]))\n(defn f [] (File. \"x\"))"}]
    (let [[row col] (f/at proj "src/a.clj" "File. ")]
      (is (= [{:java-class "java.io.File"}]
             (q/definition (:c proj) (:p proj) (f/path proj "src/a.clj") row col))))))
