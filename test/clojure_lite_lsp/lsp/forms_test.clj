(ns clojure-lite-lsp.lsp.forms-test
  "Reading just enough of the text before the cursor to know which call
  it's in, and which argument."
  (:require
   [clojure-lite-lsp.lsp.forms :as forms]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

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

(deftest dispatch-forms
  (is (= ["f" 1] (ctx "(f #_(g 1) x |")) "a discarded form isn't an argument")
  (is (= ["f" 1] (ctx "(f #_ #_ a b x |")) "nor are two")
  (is (= ["f" 1] (ctx "(f #?(:clj a :cljs b) |")) "a reader conditional is one")
  (is (= ["f" 1] (ctx "(f #?@(:clj [a b]) |")))
  (is (= ["f" 1] (ctx "(f #inst \"2020\" |")) "a tagged literal is one")
  (is (= ["f" 1] (ctx "(f #:a{:b 1} |")) "a namespaced map is one")
  (is (= ["f" 1] (ctx "(f #'x |")) "a var quote is one")
  (is (= ["f" 1] (ctx "(f #\"re\" |")) "a regex is one")
  (is (= ["f" 2] (ctx "(f ##Inf x |")) "a symbolic value is one, prefixing nothing"))

(deftest a-cursor-in-a-string-is-in-that-argument
  (is (= ["str" 0] (ctx "(str \"hello |"))))

(deftest type-hints-arent-parameters
  (is (= {:params ["s" "n"] :variadic nil} (forms/arglist-params "[^String s n]")))
  (is (= {:params ["s"] :variadic nil} (forms/arglist-params "[^{:tag String} s]")))
  (is (= {:params ["a" "more"] :variadic 1} (forms/arglist-params "[a & ^java.util.List more]"))))

(deftest the-cursor-on-the-function-name
  (is (= ["greet" 0] (ctx "(gre|et 1)")))
  (is (= ["greet" 0] (ctx "(greet| 1)"))))

(deftest the-number-of-arguments-in-the-call
  ;; the arity is chosen by the whole call, not just what's before the cursor
  (let [text "(f 1 2 3) x"]
    (is (= 3 (:count (forms/call-at text 5))))
    (is (= 1 (:arg (forms/call-at text 5)))))
  (is (= 3 (:count (forms/call-at "(f 1 (g 2) \"a)\"" 3))) "unclosed: to the end, nested forms and strings as one"))
