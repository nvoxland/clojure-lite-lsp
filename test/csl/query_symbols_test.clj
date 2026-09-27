(ns csl.query-symbols-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.query :as q]
   [csl.query-fixture :as f :refer [with-project]]))

(defn hover [proj file needle]
  (let [[row col] (f/at proj file needle)]
    (map #(dissoc % :location) (q/hover (:c proj) (:p proj) (f/path proj file) row col))))

(deftest hover-shows-docs-and-arglists
  (with-project [proj {"src/a.clj" "(ns a \"The a namespace.\")\n(defn f \"Adds.\" ([x] x) ([x y] (+ x y)))\n(defmacro ^:deprecated m [] nil)\n(f 1) (m)"}]
    (is (= [{:kind :var-def :ns "a" :name "f" :doc "Adds." :arglists ["[x]" "[x y]"] :flags #{}}]
           (hover proj "src/a.clj" "f 1")))
    (is (= #{:macro :deprecated} (:flags (first (hover proj "src/a.clj" "m)")))))
    (testing "namespaces"
      (is (= [{:kind :ns-def :ns nil :name "a" :doc "The a namespace." :flags #{}}]
             (hover proj "src/a.clj" "a \""))))
    (testing "vars from jars"
      (is (re-find #"Same as \(def name \(fn" (:doc (first (hover proj "src/a.clj" "defn"))))))))

(deftest document-symbols-in-order
  (with-project [proj {"src/a.clj" "(ns a)\n(defn f [] 1)\n(def x 2)\n(defmacro m [] nil)"}]
    (is (= [[:ns-def "a"] [:var-def "f"] [:var-def "x"] [:var-def "m"]]
           (map (juxt :kind :name) (q/document-symbols (:c proj) (:p proj) (f/path proj "src/a.clj")))))))

(deftest workspace-symbols-search-names
  (with-project [proj {"src/app/core.clj" "(ns app.core)\n(defn frobnicate [] 1)\n(defn frob [] 2)\n(defn other [] 3)"}]
    (let [search #(map (juxt :ns :name) (q/workspace-symbols (:c proj) (:p proj) % {:limit 10}))]
      (testing "substring matches, the exact name first"
        (is (= [["app.core" "frob"] ["app.core" "frobnicate"]] (search "frob"))))
      (is (= [["app.core" "frobnicate"]] (search "nicat")))
      (testing "queries too short for trigrams match prefixes, the project's own first"
        (let [found (search "fr")]
          (is (= #{["app.core" "frob"] ["app.core" "frobnicate"]} (set (take 2 found))))
          (is (some #{["clojure.core" "frequencies"]} found) "dependencies too")))
      (testing "only definitions the project can see"
        (is (empty? (search "zzzqqq"))))
      (testing "a blank query (every name matches) finds nothing rather than sorting them all"
        (is (empty? (search "")))
        (is (empty? (search "  ")))))))

(deftest call-hierarchy
  (with-project [proj {"src/a.clj" "(ns a)\n(defn leaf [] 1)\n(defn mid [] (leaf) (leaf))\n(defn top [] (mid) (str (leaf)))"
                       "src/b.clj" "(ns b (:require [a]))\n(defn other [] (a/mid))"}]
    (testing "incoming: callers, with their call sites"
      (is (= {["a" "top"] [(f/at proj "src/a.clj" "leaf)))")]
              ["a" "mid"] [(f/at proj "src/a.clj" "leaf) (leaf") (f/at proj "src/a.clj" "leaf))")]}
             (into {} (for [{:keys [caller calls]} (q/incoming-calls (:c proj) (:p proj) "a" "leaf")]
                        [[(:ns caller) (:name caller)] (sort (map #(vec (take 2 (:pos %))) calls))])))))
    (testing "incoming across files"
      (is (= #{["a" "top"] ["b" "other"]}
             (set (map (comp (juxt :ns :name) :caller) (q/incoming-calls (:c proj) (:p proj) "a" "mid"))))))
    (testing "outgoing: what a function calls, with where"
      (let [out (q/outgoing-calls (:c proj) (:p proj) "a" "top")]
        (is (= #{["a" "mid"] ["a" "leaf"] ["clojure.core" "str"]}
               (set (map (comp (juxt :ns :name) :callee) out))))
        (is (= [["src/a.clj" (f/at proj "src/a.clj" "mid [")]]
               (map #(f/rel proj %) (:locations (:callee (first (filter #(= "mid" (:name (:callee %))) out)))))))))
    (testing "the items at a position"
      (let [[row col] (f/at proj "src/a.clj" "mid [")]
        (is (= [["a" "mid"]] (map (juxt :ns :name) (q/call-hierarchy-items (:c proj) (:p proj) (f/path proj "src/a.clj") row col))))))))
