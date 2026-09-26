(ns csl.lsp.buffers-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [csl.lsp.buffers :as buffers]
   [csl.test-util :as tu]))

(defn lines [& ls] (str (str/join "\n" ls) "\n"))

(defn mapping [base current]
  (let [m (buffers/line-map base current)]
    {:to-base (mapv #(buffers/current->base m %) (range (count (str/split-lines current))))
     :to-current (mapv #(buffers/base->current m %) (range (count (str/split-lines base))))}))

(deftest unchanged-text-maps-to-itself
  (is (= {:to-base [0 1 2] :to-current [0 1 2]} (mapping (lines "a" "b" "c") (lines "a" "b" "c")))))

(deftest inserted-lines-shift-what-follows
  (is (= {:to-base [0 nil nil 1 2] :to-current [0 3 4]}
         (mapping (lines "a" "b" "c") (lines "a" "new1" "new2" "b" "c")))))

(deftest deleted-lines-map-to-nothing
  (is (= {:to-base [0 2] :to-current [0 nil 1]}
         (mapping (lines "a" "b" "c") (lines "a" "c")))))

(deftest changed-lines-map-to-nothing
  (is (= {:to-base [0 nil 2] :to-current [0 nil 2]}
         (mapping (lines "a" "b" "c") (lines "a" "B" "c")))))

(deftest edits-in-several-places
  (is (= {:to-base [nil 0 1 2 nil 4] :to-current [1 2 3 nil 5]}
         (mapping (lines "a" "b" "c" "d" "e") (lines "top" "a" "b" "c" "D" "e")))))

(deftest a-huge-rewrite-still-maps-the-unchanged-ends
  (let [n 3000
        base (apply lines (concat ["head"] (map #(str "old" %) (range n)) ["tail"]))
        current (apply lines (concat ["head"] (map #(str "new" %) (range n)) ["tail"]))
        m (buffers/line-map base current)]
    (is (= 0 (buffers/current->base m 0)))
    (is (= (inc n) (buffers/current->base m (inc n))))
    (is (nil? (buffers/current->base m 5)))))

(deftest open-documents
  (let [f (io/file (tu/temp-dir) "a.clj")
        path (str f)
        _ (spit f (lines "(ns a)" "(defn f [] 1)"))
        store (buffers/store)]
    (buffers/open! store path (slurp f))
    (testing "an unchanged buffer maps positions as they are"
      (is (= [2 7] (buffers/->indexed store path [2 7]))))
    (buffers/change! store path (lines "(ns a)" ";; new" "(defn f [] 1)"))
    (testing "edits above shift positions onto the indexed (saved) version"
      (is (= [2 7] (buffers/->indexed store path [3 7])))
      (is (= [3 7 3 8] (buffers/->buffer store path [2 7 2 8])))
      (is (nil? (buffers/->indexed store path [2 1])) "the new line doesn't exist there"))
    (testing "after a save the buffer is the base again"
      (buffers/saved! store path)
      (is (= [3 7] (buffers/->indexed store path [3 7]))))
    (testing "files that aren't open map as they are"
      (is (= [5 5] (buffers/->indexed store "/elsewhere.clj" [5 5]))))
    (buffers/close! store path)
    (is (= [9 1] (buffers/->indexed store path [9 1])))))
