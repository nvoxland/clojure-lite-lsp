(ns clojure-lite-lsp.commands-test
  "The query commands agents (and people) use: by symbol or by position,
  answered as data and as grep-like text."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.query-fixture :as f :refer [with-project]]
   [clojure-lite-lsp.test-util :as tu]))

(def files
  {"src/app/a.clj" "(ns app.a)\n(defn greet\n  \"Says hello.\"\n  [who]\n  (str \"hello \" who))\n(defn twice [x] (greet x) (greet x))\n"
   "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(defn main [] (a/greet \"you\"))\n"})

(defn ctx [proj] {:c (:c proj) :p (:p proj) :root (:root proj) :home (str (tu/temp-dir))})

(defn run [proj & args] (commands/run (ctx proj) args))

(defn text [proj & args] (commands/text (ctx proj) args))

(defn lines [s] (str/split-lines s))

(deftest definition-by-symbol-and-by-position
  (with-project [proj files]
    (is (= ["src/app/a.clj:2:7: (defn greet"] (lines (text proj "definition" "app.a/greet"))))
    (is (= ["src/app/a.clj:1:5: (ns app.a)"] (lines (text proj "definition" "app.a"))) "a namespace")
    (testing "a position (file:line:col, 1-based) means what's there"
      (is (= ["src/app/a.clj:2:7: (defn greet"] (lines (text proj "definition" (str (f/path proj "src/app/b.clj") ":2:18"))))))
    (testing "as data"
      (is (= [{:path (f/path proj "src/app/a.clj") :line 2 :column 7 :end-line 2 :end-column 12}]
             (map #(select-keys % [:path :line :column :end-line :end-column]) (:results (run proj "definition" "app.a/greet"))))))))

(deftest references-list-every-use
  (with-project [proj files]
    (is (= #{"src/app/a.clj:6:18: (defn twice [x] (greet x) (greet x))"
             "src/app/a.clj:6:28: (defn twice [x] (greet x) (greet x))"
             "src/app/b.clj:2:16: (defn main [] (a/greet \"you\"))"}
           (set (lines (text proj "references" "app.a/greet")))))))

(deftest doc-shows-arglists-and-docstring
  (with-project [proj files]
    (let [t (text proj "doc" "app.a/greet")]
      (is (str/includes? t "app.a/greet"))
      (is (str/includes? t "[who]"))
      (is (str/includes? t "Says hello.")))))

(deftest symbols-and-outline
  (with-project [proj files]
    (is (some #{"src/app/a.clj:2:7: app.a/greet"} (lines (text proj "symbols" "gree"))))
    (is (= ["src/app/a.clj:1:5: ns app.a" "src/app/a.clj:2:7: app.a/greet" "src/app/a.clj:6:7: app.a/twice"]
           (lines (text proj "outline" (f/path proj "src/app/a.clj")))))))

(deftest callers-and-callees
  (with-project [proj files]
    (is (= #{"src/app/a.clj:6:18: app.a/twice calls it"
             "src/app/a.clj:6:28: app.a/twice calls it"
             "src/app/b.clj:2:16: app.b/main calls it"}
           (set (lines (text proj "callers" "app.a/greet")))))
    (is (= #{"src/app/a.clj:6:18: calls app.a/greet" "src/app/a.clj:6:28: calls app.a/greet"}
           (set (lines (text proj "callees" "app.a/twice")))))))

(deftest nothing-found-says-so
  (with-project [proj files]
    (is (= "No results." (text proj "references" "app.a/nope")))))

(deftest help-lists-every-command
  (let [h (commands/help)]
    (doseq [c ["definition" "references" "implementations" "doc" "callers" "callees" "symbols" "outline"]]
      (is (str/includes? h c) c))))

(deftest unknown-commands-are-refused
  (with-project [proj files]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown query" (run proj "frobnicate" "x")))))
