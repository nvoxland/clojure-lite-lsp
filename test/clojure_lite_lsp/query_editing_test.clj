(ns clojure-lite-lsp.query-editing-test
  "What the editor asks while you work in a file: the occurrences of what's
  under the cursor."
  (:require
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.query-fixture :as qf :refer [with-project]]
   [clojure.test :refer [deftest is testing]]))

(defn- highlights
  "The occurrences highlighted from the `n`th (default 0th) `needle` in
  `file`: {[row col] :write or :read}."
  [proj file needle & {:keys [n] :or {n 0}}]
  (let [[row col] (qf/at proj file needle n)]
    (into {}
          (map (fn [{:keys [pos write?]}] [(subvec pos 0 2) (if write? :write :read)]))
          (q/highlights (:c proj) (:p proj) (qf/path proj file) row col))))

(defn- starts
  "Where each of `needles` first occurs in `file`."
  [proj file & needles]
  (set (map #(qf/at proj file %) needles)))

(deftest highlights-are-the-files-own-occurrences
  (with-project [proj {"src/app/a.clj" (str "(ns app.a)\n"
                                            "(defn greet [who]\n"
                                            "  (str who who))\n"
                                            "(defn twice [x] (greet x) (greet x))\n"
                                            "(def m {:k 1})\n"
                                            "(:k m)\n")
                       "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"}]
    (let [at #(qf/at proj "src/app/a.clj" %1 %2)
          highlights #(highlights proj "src/app/a.clj" %)]
      (testing "a var: its definition and uses in this file, not other files'"
        (is (= {(at "greet [" 0) :write (at "greet x" 0) :read (at "greet x" 1) :read}
               (highlights "greet x")
               (highlights "greet ["))))
      (testing "a local: its binding and uses"
        (is (= {(at "who]" 0) :write (at "who who" 0) :read (at "who))" 0) :read}
               (highlights "who who"))))
      (testing "a keyword"
        (is (= {(at ":k 1" 0) :read (at ":k m" 0) :read}
               (highlights ":k m")))))))

(deftest a-cljc-local-is-one-local-in-both-languages
  ;; clj-kondo analyzes a .cljc file once per language, giving the same
  ;; local an id in each: its highlights are the uses in both branches
  (with-project [proj {"src/app/c.cljc" "(ns app.c)\n(defn f [x] #?(:clj (inc x) :cljs (dec x)))\n"}]
    (doseq [from ["x]" "x) :cljs" "x)))"]]
      (testing (str "from " from)
        (is (= (starts proj "src/app/c.cljc" "x]" "x) :cljs" "x)))")
               (set (keys (highlights proj "src/app/c.cljc" from)))))))))

(deftest highlights-of-a-keys-binding-are-the-locals
  ;; clj-kondo records both a local and a keyword at a :keys binding: the
  ;; cursor there means the local, not every :a in the file
  (with-project [proj {"src/app/k.clj" "(ns app.k)\n(defn f [m] (let [{:keys [a]} m] a))\n(:a {})\n"}]
    (is (= (starts proj "src/app/k.clj" "a]}" "a))")
           (set (keys (highlights proj "src/app/k.clj" "a]}")))))))

(deftest highlights-of-an-alias-are-its-uses
  (with-project [proj {"src/app/s.clj" "(ns app.s (:require [clojure.string :as str]))\n(str/join [])\n(str/blank? \"\")\n"}]
    (is (= (starts proj "src/app/s.clj" "str]" "str/join" "str/blank")
           (set (keys (highlights proj "src/app/s.clj" "str]")))))))
