(ns clojure-lite-lsp.query-editing-test
  "What the editor asks while you work in a file: the occurrences of what's
  under the cursor, and the places a local's rename touches."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.query-fixture :as f :refer [with-project]]))

(def files
  {"src/app/a.clj" (str "(ns app.a)\n"
                        "(defn greet [who]\n"
                        "  (str who who))\n"
                        "(defn twice [x] (greet x) (greet x))\n"
                        "(def m {:k 1})\n"
                        "(:k m)\n")
   "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})

(defn at [proj needle n] (f/at proj "src/app/a.clj" needle n))

(defn highlights [proj needle n]
  (let [[row col] (at proj needle n)]
    (set (map (fn [{:keys [pos write?]}] [(vec (take 2 pos)) (if write? :write :read)])
              (q/highlights (:c proj) (:p proj) (f/path proj "src/app/a.clj") row col)))))

(deftest highlights-are-the-files-own-occurrences
  (with-project [proj files]
    (testing "a var: its definition and uses in this file, not other files'"
      (is (= #{[(at proj "greet [" 0) :write] [(at proj "greet x" 0) :read] [(at proj "greet x" 1) :read]}
             (highlights proj "greet x" 0)
             (highlights proj "greet [" 0))))
    (testing "a local: its binding and uses"
      (is (= #{[(at proj "who]" 0) :write] [(at proj "who who" 0) :read] [(at proj "who))" 0) :read]}
             (highlights proj "who who" 0))))
    (testing "a keyword"
      (is (= #{[(at proj ":k 1" 0) :read] [(at proj ":k m" 0) :read]}
             (highlights proj ":k m" 0))))))

(deftest a-locals-occurrences-are-what-its-rename-touches
  (with-project [proj files]
    (let [[row col] (at proj "x) (greet" 0)
          {:keys [name positions]} (q/local-occurrences (:c proj) (:p proj) (f/path proj "src/app/a.clj") row col)]
      (is (= "x" name))
      (is (= #{(at proj "x]" 0) (at proj "x) (greet" 0) (at proj "x))" 0)}
             (set (map #(vec (take 2 %)) positions)))))
    (testing "anything but a local: nil (renaming it would touch other files)"
      (let [[row col] (at proj "greet x" 0)]
        (is (nil? (q/local-occurrences (:c proj) (:p proj) (f/path proj "src/app/a.clj") row col)))))))

(deftest a-cljc-local-is-one-local-in-both-languages
  ;; clj-kondo analyzes a .cljc file once per language, giving the same
  ;; local an id in each: a rename must take the uses in both branches
  (with-project [proj {"src/app/c.cljc" "(ns app.c)\n(defn f [x] #?(:clj (inc x) :cljs (dec x)))\n"}]
    (let [path (f/path proj "src/app/c.cljc")
          all #{(f/at proj "src/app/c.cljc" "x]") (f/at proj "src/app/c.cljc" "x) :cljs") (f/at proj "src/app/c.cljc" "x)))")}]
      (doseq [from ["x]" "x) :cljs" "x)))"]
              :let [[row col] (f/at proj "src/app/c.cljc" from)]]
        (testing (str "from " from)
          (is (= all (set (map #(vec (take 2 %)) (:positions (q/local-occurrences (:c proj) (:p proj) path row col))))))
          (is (= all (set (map #(vec (take 2 (:pos %))) (q/highlights (:c proj) (:p proj) path row col))))))))))

(deftest highlights-of-a-keys-binding-are-the-locals
  ;; clj-kondo records both a local and a keyword at a :keys binding: the
  ;; cursor there means the local, not every :a in the file
  (with-project [proj {"src/app/k.clj" "(ns app.k)\n(defn f [m] (let [{:keys [a]} m] a))\n(:a {})\n"}]
    (let [path (f/path proj "src/app/k.clj")
          [row col] (f/at proj "src/app/k.clj" "a]}")]
      (is (= #{(f/at proj "src/app/k.clj" "a]}") (f/at proj "src/app/k.clj" "a))")}
             (set (map #(vec (take 2 (:pos %))) (q/highlights (:c proj) (:p proj) path row col))))))))

(deftest highlights-of-an-alias-are-its-uses
  (with-project [proj {"src/app/s.clj" "(ns app.s (:require [clojure.string :as str]))\n(str/join [])\n(str/blank? \"\")\n"}]
    (let [path (f/path proj "src/app/s.clj")
          [row col] (f/at proj "src/app/s.clj" "str]")]
      (is (= #{(f/at proj "src/app/s.clj" "str]") (f/at proj "src/app/s.clj" "str/join") (f/at proj "src/app/s.clj" "str/blank")}
             (set (map #(vec (take 2 (:pos %))) (q/highlights (:c proj) (:p proj) path row col))))))))
