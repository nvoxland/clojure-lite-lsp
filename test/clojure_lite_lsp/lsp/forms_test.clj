(ns clojure-lite-lsp.lsp.forms-test
  "Reading just enough of the text before the cursor to know which call
  it's in, and which argument."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.lsp.forms :as forms]))

(defn ctx
  "The call context at | in `s`: [head-symbol arg-index], or nil."
  [s]
  (let [i (str/index-of s "|")
        text (str/replace s "|" "")]
    (when-let [{[start end] :head :keys [arg]} (forms/call-at text i)]
      [(subs text start end) arg])))

(deftest the-enclosing-call-and-argument
  (is (= ["greet" 0] (ctx "(greet |")))
  (is (= ["greet" 1] (ctx "(greet x |")))
  (is (= ["greet" 0] (ctx "(greet x|")) "in the middle of an argument: that argument")
  (is (= ["greet" 0] (ctx "(greet |x)")))
  (is (= ["a/greet" 1] (ctx "(a/greet 1 |)")))
  (is (= ["g" 0] (ctx "(f (g |")) "the innermost call")
  (is (= ["f" 1] (ctx "(f (g x) |")))
  (is (= ["f" 0] (ctx "(f [a |")) "inside a vector argument: the call it's in")
  (is (= ["f" 1] (ctx "(f [a b] |")))
  (is (= ["f" 0] (ctx "#(f |")) "a function literal"))

(deftest what-doesnt-count
  (testing "strings, comments and character literals"
    (is (= ["str" 1] (ctx "(str \"a (b\" |")))
    (is (= ["str" 1] (ctx "(str \"a \\\" (b\" |")) "an escaped quote")
    (is (= ["f" 0] (ctx "(f ; (comment\n |")))
    (is (= ["f" 1] (ctx "(f \\( |"))))
  (testing "reader prefixes belong to the form they prefix"
    (is (= ["f" 2] (ctx "(f 'x @y |")))
    (is (= ["f" 1] (ctx "(f #{1 2} |")))
    (is (= ["f" 1] (ctx "(f ^:meta x |")) "metadata isn't an argument"))
  (testing "no call"
    (is (nil? (ctx "|")))
    (is (nil? (ctx "[a |")))
    (is (nil? (ctx "((f) |")) "a head that isn't a symbol")
    (is (nil? (ctx "(f x) |")) "after the call")))

(deftest arglist-parameters
  (is (= {:params ["a" "b" "more"] :variadic 2} (forms/arglist-params "[a b & more]")))
  (is (= {:params ["{:keys [a b]}" "c"] :variadic nil} (forms/arglist-params "[{:keys [a b]} c]")))
  (is (= {:params [] :variadic nil} (forms/arglist-params "[]"))))
