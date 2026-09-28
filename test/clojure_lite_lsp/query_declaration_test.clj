(ns clojure-lite-lsp.query-declaration-test
  "Go to declaration: where the file brings a name in (its require), where
  go to definition goes to where it's defined."
  (:require
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.query-fixture :as f :refer [with-project]]
   [clojure-lite-lsp.query-references-test :refer [locs]]
   [clojure.test :refer [deftest is testing]]))

(defn declaration [proj file needle & [n]]
  (let [[row col] (f/at proj file needle (or n 0))]
    (set (map #(f/rel proj %) (q/declaration (:c proj) (:p proj) (f/path proj file) row col)))))

(def files
  {"src/a.clj" "(ns a) (defn f [] 1) (defn g [] 2)"
   "src/b.clj" "(ns b (:require [a :as al] [a :refer [f]]))\n(al/f) (f) (a/g) (defn h [] (h)) ::al/k"})

(deftest declarations
  (with-project [proj files]
    (testing "through an alias: the alias"
      (is (= (locs proj ["src/b.clj" "al]"]) (declaration proj "src/b.clj" "al/f"))))
    (testing "a keyword through an alias: the alias"
      (is (= (locs proj ["src/b.clj" "al]"]) (declaration proj "src/b.clj" "::al/k"))))
    (testing "a referred name: its :refer entry"
      (is (= (locs proj ["src/b.clj" "f]"]) (declaration proj "src/b.clj" "f) (a"))))
    (testing "fully qualified: the namespace's require"
      (is (= (locs proj ["src/b.clj" "a :as"]) (declaration proj "src/b.clj" "a/g"))))
    (testing "the file's own var: its definition"
      (is (= (locs proj ["src/b.clj" "h []"]) (declaration proj "src/b.clj" "h))"))))
    (testing "in the require itself: the definition"
      (is (= (locs proj ["src/a.clj" "f ["]) (declaration proj "src/b.clj" "f]"))))))
