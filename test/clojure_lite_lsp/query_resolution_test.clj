(ns clojure-lite-lsp.query-resolution-test
  "Names clj-kondo leaves unresolved or resolves by guessing, which the
  index can still answer."
  (:require
   [clojure-lite-lsp.query-fixture :as f :refer [with-project]]
   [clojure-lite-lsp.query-languages-test :refer [definition]]
   [clojure-lite-lsp.query-references-test :refer [references locs]]
   [clojure.test :refer [deftest is testing]]))

(deftest fully-qualified-calls-without-a-require
  ;; clj-kondo reports each unresolved namespace once per file, unless
  ;; asked for duplicates: every call must be found
  (with-project [proj {"src/b.clj" "(ns b) (defn f [] 1) (defn g [] 2)"
                       "src/a.clj" "(ns a) (b/f) (b/g) (b/f)"}]
    (is (= (locs proj ["src/b.clj" "g ["]) (definition proj "src/a.clj" "b/g")))
    (is (= (locs proj ["src/a.clj" "b/f" 0] ["src/a.clj" "b/f" 1]) (references proj "src/b.clj" "f [")))))

(deftest vars-from-the-second-refer-all
  ;; clj-kondo attributes a name to the first :refer :all namespace
  (with-project [proj {"src/b.clj" "(ns b) (defn bfn [] 1)"
                       "src/c.clj" "(ns c) (defn cfn [] 2)"
                       "src/a.clj" "(ns a (:require [b :refer :all] [c :refer :all])) (bfn) (cfn)"}]
    (is (= (locs proj ["src/c.clj" "cfn ["]) (definition proj "src/a.clj" "cfn)")))
    (is (= (locs proj ["src/b.clj" "bfn ["]) (definition proj "src/a.clj" "bfn)")))
    (is (= (locs proj ["src/a.clj" "cfn)"]) (references proj "src/c.clj" "cfn [")))
    (testing ":use refers all too"
      (with-project [proj {"src/b.clj" "(ns b) (defn bfn [] 1)"
                           "src/c.clj" "(ns c) (defn cfn [] 2)"
                           "src/a.clj" "(ns a (:use [b] [c])) (cfn)"}]
        (is (= (locs proj ["src/c.clj" "cfn ["]) (definition proj "src/a.clj" "cfn)")))))))

(deftest a-require-after-a-use-refers-nothing
  (with-project [proj {"src/b.clj" "(ns b) (defn f [] 1)"
                       "src/c.clj" "(ns c) (defn f [] 2)"
                       "src/a.clj" "(ns a (:use [b]) (:require [c :as cc])) (f)"}]
    (is (= (locs proj ["src/b.clj" "f ["]) (definition proj "src/a.clj" "f)")))
    (is (= #{} (references proj "src/c.clj" "f [")))))

(deftest quoted-symbols-of-namespaces-not-required
  (with-project [proj {"src/b.clj" "(ns b) (defn f [] 1)"
                       "src/a.clj" "(ns a) (requiring-resolve 'b/f)"}]
    (is (= (locs proj ["src/b.clj" "f ["]) (definition proj "src/a.clj" "b/f")))
    (is (= (locs proj ["src/a.clj" "b/f"]) (references proj "src/b.clj" "f [")))))

(deftest symbols-in-edn-files
  ;; deps.edn :exec-fn, integrant and component configs
  (with-project [proj {"src/my/app.clj" "(ns my.app) (defn handler [] 1)"
                       "src/config.edn" "{:handler my.app/handler}"}]
    (is (= (locs proj ["src/my/app.clj" "handler ["]) (definition proj "src/config.edn" "my.app/handler")))
    (is (= (locs proj ["src/config.edn" "my.app/handler"]) (references proj "src/my/app.clj" "handler [")))))

(deftest a-record-used-as-a-class
  (with-project [proj {"src/a/core.clj" "(ns a.core) (defrecord R [x])"
                       "src/b.clj" "(ns b (:require [a.core]) (:import [a.core R])) (R. 1) (instance? R nil)"}]
    (is (= (locs proj ["src/a/core.clj" "R ["]) (definition proj "src/b.clj" "R. 1")))
    (is (contains? (references proj "src/a/core.clj" "R [") ["src/b.clj" (f/at proj "src/b.clj" "R. 1")]))))

(deftest references-through-potemkin-imports
  (with-project [proj {"src/impl.clj" "(ns impl) (defn f [] 1)"
                       "src/api.clj" "(ns api (:require [potemkin :refer [import-vars]] [impl])) (import-vars [impl f])"
                       "src/use.clj" "(ns use (:require [api])) (api/f)"}]
    (testing "a call through the api namespace is a reference of the var it imports"
      (is (contains? (references proj "src/impl.clj" "f [") ["src/use.clj" (f/at proj "src/use.clj" "api/f")]))
      (is (contains? (references proj "src/use.clj" "api/f") ["src/use.clj" (f/at proj "src/use.clj" "api/f")])))))
