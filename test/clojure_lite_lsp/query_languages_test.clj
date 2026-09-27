(ns clojure-lite-lsp.query-languages-test
  "A namespace with a .clj and a .cljs file: answers keep to the language
  asked about."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.query-fixture :as f :refer [with-project]]
   [clojure-lite-lsp.query-references-test :refer [references locs]]))

(def files
  {"src/app/util.clj" "(ns app.util)\n(defn fmt [x] x)"
   "src/app/util.cljs" "(ns app.util)\n(defn fmt [x] x)"
   "src/app/a.clj" "(ns app.a (:require [app.util :as u]))\n(u/fmt 1)"
   "src/app/b.cljs" "(ns app.b (:require [app.util :as u]))\n(u/fmt 2)"
   "src/app/m.clj" "(ns app.m)\n(defmacro mac [x] x)"
   "src/app/c.cljs" "(ns app.c (:require-macros [app.m :as m]))\n(m/mac 1)"})

(defn definition [proj file needle & [offset]]
  (let [[row col] (f/at proj file needle)]
    (set (map #(f/rel proj %) (q/definition (:c proj) (:p proj) (f/path proj file) row (+ col (or offset 0)))))))

(deftest references-keep-to-their-language
  (with-project [proj files]
    (is (= (locs proj ["src/app/a.clj" "u/fmt"]) (references proj "src/app/util.clj" "fmt [")))
    (is (= (locs proj ["src/app/b.cljs" "u/fmt"]) (references proj "src/app/util.cljs" "fmt [")))
    (testing "with the declaration: its own"
      (is (= (locs proj ["src/app/a.clj" "u/fmt"] ["src/app/util.clj" "fmt ["])
             (references proj "src/app/util.clj" "fmt [" :include-declaration? true))))
    (testing "a clj macro is used from cljs"
      (is (= (locs proj ["src/app/c.cljs" "m/mac"]) (references proj "src/app/m.clj" "mac ["))))))

(deftest namespace-jumps-keep-to-their-language
  (with-project [proj files]
    (is (= (locs proj ["src/app/util.cljs" "app.util"]) (definition proj "src/app/b.cljs" "app.util")))
    (is (= (locs proj ["src/app/util.clj" "app.util"]) (definition proj "src/app/a.clj" "app.util")))))

(deftest a-cursor-just-after-a-name-finds-it
  ;; editors put the cursor between characters: right after the name is
  ;; still on it
  (with-project [proj files]
    (is (= (locs proj ["src/app/util.clj" "fmt ["]) (definition proj "src/app/a.clj" "u/fmt" (count "u/fmt"))))))

(deftest namespace-references-cross-languages-only-where-the-namespace-doesnt
  (with-project [proj files]
    (testing "a clj-only macro namespace: cljs files requiring its macros use it"
      (is (contains? (references proj "src/app/m.clj" "app.m")
                     ["src/app/c.cljs" (f/at proj "src/app/c.cljs" "app.m")])))
    (testing "a namespace with a file per language: each keeps its own"
      (is (= #{"src/app/a.clj"} (set (map first (references proj "src/app/util.clj" "app.util")))))
      (is (= #{"src/app/b.cljs"} (set (map first (references proj "src/app/util.cljs" "app.util"))))))))
