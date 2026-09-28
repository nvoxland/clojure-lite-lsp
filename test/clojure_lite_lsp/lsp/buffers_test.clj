(ns clojure-lite-lsp.lsp.buffers-test
  (:require
   [clojure-lite-lsp.lsp.buffers :as buffers]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

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
    (buffers/change! store path [{:text (lines "(ns a)" ";; new" "(defn f [] 1)")}])
    (testing "edits above shift positions onto the indexed (saved) version"
      (is (= [2 7] (buffers/->indexed store path [3 7])))
      (is (= [3 7 3 8] (buffers/->buffer store path [2 7 2 8])))
      (is (nil? (buffers/->indexed store path [2 1])) "the new line doesn't exist there"))
    (testing "after a save, until the index has it, positions still map onto what it has"
      (buffers/saved! store path)
      (is (= [2 7] (buffers/->indexed store path [3 7])))
      (is (= [path] (keys (buffers/awaiting store)))))
    (testing "once indexed, the saved buffer is the base"
      (buffers/indexed! store path)
      (is (= [3 7] (buffers/->indexed store path [3 7])))
      (is (empty? (buffers/awaiting store))))
    (testing "files that aren't open map as they are"
      (is (= [5 5] (buffers/->indexed store "/elsewhere.clj" [5 5]))))
    (buffers/close! store path)
    (is (= [9 1] (buffers/->indexed store path [9 1])))))

(deftest a-position-past-the-end-of-a-line-is-its-end
  ;; LSP: a character beyond the line's length means the line's end
  (let [f (io/file (tu/temp-dir) "a.clj")
        path (str f)
        store (buffers/store)]
    (spit f (lines "abc" "def"))
    (buffers/open! store path (slurp f))
    (buffers/change! store path [{:range {:start {:line 0 :character 1} :end {:line 0 :character 99}} :text "X"}])
    (is (= (lines "aX" "def") (:current (@store path))))))

(deftest incremental-changes
  (let [text "(ns a)\n(defn f [] 1)\n"
        change (fn [t & changes] (reduce buffers/apply-change t changes))]
    (testing "a range replaced"
      (is (= "(ns a)\n(defn g [] 1)\n"
             (change text {:range {:start {:line 1 :character 6} :end {:line 1 :character 7}} :text "g"}))))
    (testing "an insertion spanning lines, then a deletion"
      (is (= "(ns a)\n;; x\n(defn f [] 2)\n"
             (change text
                     {:range {:start {:line 1 :character 0} :end {:line 1 :character 0}} :text ";; x\n"}
                     {:range {:start {:line 2 :character 11} :end {:line 2 :character 12}} :text "2"}))))
    (testing "characters are UTF-16 units, as Java strings count them"
      (is (= "(def s \"😀\") (def t 1)"
             (change "(def s \"😀\") (def u 1)" {:range {:start {:line 0 :character 18} :end {:line 0 :character 19}} :text "t"}))))
    (testing "no range: the whole text"
      (is (= "new" (change text {:text "new"}))))))
