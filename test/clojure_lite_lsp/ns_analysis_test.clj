(ns clojure-lite-lsp.ns-analysis-test
  "Hooks that ask clj-kondo about another namespace (hooks-api/ns-analysis)
  get their answer from the index, and the files they ran in are analyzed
  again when that answer changes."
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.index-fixture :refer [analyzed-files sync-project! temp-indexer visible-defs]]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.ns-analysis :as nsa]
   [clojure-lite-lsp.test-util :as tu :refer [project! jar!]]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def reexport-hook
  "Like dtype-next's export-symbols: defines the listed vars that the
  source namespace has."
  "(ns hooks.re (:require [clj-kondo.hooks-api :as api]))
   (defmacro reexport [src & syms]
     (let [defs (:clj (api/ns-analysis src))]
       (list* 'do (for [s syms :when (get defs s)] (list 'def s nil)))))")

(def config "{:hooks {:macroexpand {app.re/reexport hooks.re/reexport}}}")

(defn reexport-project [src]
  (project! {"deps.edn" "{:paths [\"src\"]}"
             ".clj-kondo/config.edn" config
             ".clj-kondo/hooks/re.clj" reexport-hook
             "src/app/re.clj" "(ns app.re) (defmacro reexport [src & syms])"
             "src/app/src.clj" src
             "src/app/api.clj" "(ns app.api (:require [app.re :refer [reexport]])) (reexport app.src f g h)"}))

(def src-fg "(ns app.src) (defn f ([a] a) ([a b] b)) (defn g [& xs] xs)")
(def src-fgh (str src-fg " (defn h [] 1)"))

(defn api-defs [defs] (set (filter #(re-find #"^app\.api/" %) defs)))

(deftest answers-have-clj-kondos-shape
  (is (= {:clj '{f {:ns app.src :name f :fixed-arities #{1 2}}
                 g {:ns app.src :name g :varargs-min-arity 0}
                 m {:ns app.src :name m :fixed-arities #{1} :macro true}
                 p {:ns app.src :name p :fixed-arities #{0} :private true}}}
         (nsa/answer :clj
                     [{:ext "clj" :ns "app.src" :name "f" :lang #{:clj} :flags #{} :extra {:fixed-arities #{1 2}}}
                      {:ext "clj" :ns "app.src" :name "g" :lang #{:clj} :flags #{} :extra {:varargs-min-arity 0}}
                      {:ext "clj" :ns "app.src" :name "m" :lang #{:clj} :flags #{:macro} :extra {:fixed-arities #{1}}}
                      {:ext "clj" :ns "app.src" :name "p" :lang #{:clj} :flags #{:private} :extra {:fixed-arities #{0}}}])))
  (testing "a .cljc file answers per language"
    (is (= {:clj '{f {:ns app.src :name f}} :cljs '{f {:ns app.src :name f} k {:ns app.src :name k}}}
           (nsa/answer :cljc [{:ext "cljc" :ns "app.src" :name "f" :lang #{:clj :cljs} :flags #{} :extra {}}
                              {:ext "cljc" :ns "app.src" :name "k" :lang #{:cljs} :flags #{} :extra {}}]))))
  (testing "other file types don't answer for the language"
    (is (nil? (nsa/answer :clj [{:ext "cljc" :ns "app.src" :name "f" :lang #{:clj} :flags #{} :extra {}}])))))

(deftest a-namespace-analyzed-in-the-same-batch
  (let [root (reexport-project src-fg)
        batches (atom 0)
        real analyze/analyze-files]
    (with-open [ix (temp-indexer)]
      (with-redefs [analyze/analyze-files (fn [paths opts]
                                            (when (some #(re-find #"api\.clj$" %) paths) (swap! batches inc))
                                            (real paths opts))]
        (let [p (sync-project! ix root)]
          (is (= #{"app.api/f" "app.api/g"} (api-defs (visible-defs (:c ix) p))))
          (is (= 1 @batches) "settled within the batch, not by queueing it again"))))))

(deftest a-namespace-analyzed-in-an-earlier-batch
  (let [root (reexport-project src-fg)]
    (with-open [ix (temp-indexer :batch-sizes {:file 1})]
      (let [p (sync-project! ix root)]
        (is (= #{"app.api/f" "app.api/g"} (api-defs (visible-defs (:c ix) p))))))))

(deftest editing-the-namespace-reanalyzes-the-files-that-asked
  (let [root (reexport-project src-fg)]
    (with-open [ix (temp-indexer)]
      (let [p (sync-project! ix root)]
        (is (= #{"src.clj" "api.clj"}
               (analyzed-files ix root #(spit (io/file root "src/app/src.clj") src-fgh))))
        (is (= #{"app.api/f" "app.api/g" "app.api/h"} (api-defs (visible-defs (:c ix) p))))
        (testing "a change the answer doesn't show reanalyzes only that file"
          (is (= #{"src.clj"}
                 (analyzed-files ix root #(spit (io/file root "src/app/src.clj") (str src-fgh " ;; comment"))))))))))

(deftest worktrees-with-different-answers-each-get-theirs
  (let [a (reexport-project src-fg)
        b (reexport-project src-fgh)]
    (with-open [ix (temp-indexer)]
      (let [pa (sync-project! ix a)
            pb (sync-project! ix b)]
        (is (= #{"app.api/f" "app.api/g"} (api-defs (visible-defs (:c ix) pa))))
        (is (= #{"app.api/f" "app.api/g" "app.api/h"} (api-defs (visible-defs (:c ix) pb))))
        (testing "and a third like the first reuses its analysis"
          (let [c (reexport-project src-fg)]
            (is (= #{} (analyzed-files ix c (fn []))))))))))

(deftest a-jar-asking-about-its-own-namespaces
  (let [jar (jar! {"clj-kondo.exports/acme/lib/config.edn" "{:hooks {:macroexpand {acme.re/reexport acme.hooks/reexport}}}"
                   "clj-kondo.exports/acme/lib/acme/hooks.clj" (str/replace reexport-hook "hooks.re" "acme.hooks")
                   "acme/re.clj" "(ns acme.re) (defmacro reexport [src & syms])"
                   "acme/impl.clj" "(ns acme.impl) (defn f [a] a) (defn g [] 1)"
                   "acme/api.clj" "(ns acme.api (:require [acme.re :refer [reexport]])) (reexport acme.impl f g h)"})
        cfg (kc/jar-config! (tu/temp-dir) (kc/jar-context [jar]) jar)
        [{:keys [entries]}] (analyze/analyze-jars [jar] {:configs {jar cfg} :jar-hashes {jar (byte-array 1)}})
        defs (set (for [{:keys [elements]} entries e elements :when (= :var-def (:kind e))] (str (:ns e) "/" (:name e))))]
    (is (= #{"acme.api/f" "acme.api/g"} (set (filter #(re-find #"^acme\.api/" %) defs))))))

(deftest answers-that-never-settle-dont-loop
  ;; were a file's analysis never to match today's answers (here: keys
  ;; that ignore them, so re-analysis finds the same stale unit), the
  ;; daemon must still go idle
  (let [root (reexport-project src-fg)
        key-hash writer/unit-key-hash]
    (with-redefs [writer/unit-key-hash (fn [k] (key-hash (dissoc k :ns-deps)))]
      (with-open [ix (temp-indexer)]
        (sync-project! ix root)
        (spit (io/file root "src/app/src.clj") src-fgh)
        (let [err (java.io.StringWriter.)
              done (binding [*err* err] (future (sync-project! ix root) :idle))]
          (try
            (is (= :idle (deref done 60000 :still-looping)))
            (is (str/includes? (str err) "doesn't match what its hooks are told") "and says so")
            (finally (future-cancel done))))))))

(deftest questions-from-hooks-that-only-lint-count-too
  ;; a hook that returns the node it was given this time may change it
  ;; given another answer
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        ".clj-kondo/config.edn" "{:hooks {:analyze-call {app.re/check hooks.check/check}}}"
                        ".clj-kondo/hooks/check.clj" "(ns hooks.check (:require [clj-kondo.hooks-api :as api]))
                                                      (defn check [{:keys [node]}] (api/ns-analysis 'app.src) {:node node})"
                        "src/app/re.clj" "(ns app.re) (defmacro check [x] x)"
                        "src/app/src.clj" src-fg
                        "src/app/checked.clj" "(ns app.checked (:require [app.re :as re])) (re/check 1)"})]
    (with-open [ix (temp-indexer)]
      (sync-project! ix root)
      (is (= #{"src.clj" "checked.clj"} (analyzed-files ix root #(spit (io/file root "src/app/src.clj") src-fgh)))))))

(deftest a-namespace-spread-over-files-is-answered-whole
  ;; (in-ns 'app.src) in a second file: clj-kondo counts its vars as the
  ;; namespace's too, so the answer has both files' vars
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        ".clj-kondo/config.edn" config
                        ".clj-kondo/hooks/re.clj" reexport-hook
                        "src/app/re.clj" "(ns app.re) (defmacro reexport [src & syms])"
                        "src/app/src.clj" "(ns app.src) (defn f [a] a)"
                        "src/app/src_more.clj" "(in-ns 'app.src) (defn g [] 1)"
                        "src/app/api.clj" "(ns app.api (:require [app.re :refer [reexport]])) (reexport app.src f g h)"})]
    (with-open [ix (temp-indexer :batch-sizes {:file 1})]
      (let [p (sync-project! ix root)]
        (is (= #{"app.api/f" "app.api/g"} (api-defs (visible-defs (:c ix) p))))))))

(deftest the-requeue-guard-catches-cycles
  ;; analysis that alternates between two results must stop too
  (let [seen (atom {})]
    (is (true? (indexer/first-requeue! seen [1 "a"] [10 ["x"]])))
    (is (true? (indexer/first-requeue! seen [1 "a"] [11 ["y"]])))
    (is (false? (indexer/first-requeue! seen [1 "a"] [10 ["x"]])) "back to the first: a cycle")
    (is (true? (indexer/first-requeue! seen [1 "b"] [10 ["x"]])) "per file")))
