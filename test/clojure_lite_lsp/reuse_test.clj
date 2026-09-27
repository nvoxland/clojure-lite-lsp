(ns clojure-lite-lsp.reuse-test
  "Analysis reused across clj-kondo configs: a change to the config
  re-analyzes only the files that use what changed."
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.indexer-test :refer [visible-defs sync-project!]]
   [clojure-lite-lsp.test-util :as tu]))

(defn config [m] (pr-str m))

(def lint-as '{acme/defthing clojure.core/def acme/defother clojure.core/def})

(defn fixture []
  (project! {"deps.edn" "{:paths [\"src\"]}"
             ".clj-kondo/config.edn" (config {:lint-as lint-as})
             "src/acme.clj" "(ns acme) (defmacro defthing [n v] `(def ~n ~v)) (defmacro defother [n v] `(def ~n ~v))"
             "src/app/uses_thing.clj" "(ns app.uses-thing (:require [acme])) (acme/defthing thing 1)"
             "src/app/uses_other.clj" "(ns app.uses-other (:require [acme])) (acme/defother other 1)"
             "src/app/plain.clj" "(ns app.plain) (defn plain [] 1)"}))

(defn analyzed-files
  "Re-sync the project after `change`, returning the names of the files
  analyzed."
  [ix root change]
  (change)
  (let [seen (atom #{})
        real analyze/analyze-files]
    (with-redefs [analyze/analyze-files (fn [paths opts]
                                          (swap! seen into (map #(.getName (io/file ^String %)) paths))
                                          (real paths opts))]
      (sync-project! ix root))
    @seen))

(defn set-config! [root m] (spit (io/file root ".clj-kondo/config.edn") (config m)))

(deftest a-change-for-one-macro-reanalyzes-its-users-only
  (let [root (fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [p (sync-project! ix root)]
        (is (= #{"uses_other.clj"}
               (analyzed-files ix root #(set-config! root {:lint-as (assoc lint-as 'acme/defother 'clojure.core/defonce)}))))
        (testing "the results are those of the new config"
          (is (every? (visible-defs (:c ix) p) ["app.uses-thing/thing" "app.uses-other/other" "app.plain/plain"])))
        (testing "and going back reuses the first analysis"
          (is (= #{} (analyzed-files ix root #(set-config! root {:lint-as lint-as})))))))))

(deftest linter-settings-reanalyze-nothing
  (let [root (fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (sync-project! ix root)
      (is (= #{} (analyzed-files ix root #(set-config! root {:lint-as lint-as
                                                             :linters {:unused-binding {:level :off}}})))))))

(deftest a-global-change-reanalyzes-everything
  (let [root (fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (sync-project! ix root)
      (is (= #{"acme.clj" "uses_thing.clj" "uses_other.clj" "plain.clj"}
             (analyzed-files ix root #(set-config! root {:lint-as lint-as
                                                         :ns-groups [{:pattern "app\\..*" :name 'app-group}]
                                                         :config-in-ns {'app-group {:lint-as '{acme/y clojure.core/def}}}})))))))

(def lint-hook
  "(ns hooks.lint (:require [clj-kondo.hooks-api :as api]))
   (defn check [{:keys [node config]}] (when (:acme/strict config) (count (:children node))) {:node node})")

(def transform-hook
  "(ns hooks.xform (:require [clj-kondo.hooks-api :as api]))
   (defn expand [{:keys [node config]}]
     (let [[_ n v] (:children node)]
       {:node (api/list-node [(api/token-node 'def) n (if (:acme/strict config) v (api/token-node nil))])}))")

(def plain-transform-hook
  "A transforming hook that reads no config."
  "(ns hooks.plainx (:require [clj-kondo.hooks-api :as api]))
   (defn expand [{:keys [node]}]
     (let [[_ n v] (:children node)] {:node (api/list-node [(api/token-node 'def) n v])}))")

(defn hooks-fixture []
  (project! {"deps.edn" "{:paths [\"src\"]}"
             ".clj-kondo/config.edn" (config '{:hooks {:analyze-call {acme/checked hooks.lint/check acme/defx hooks.xform/expand
                                                                     acme/defp hooks.plainx/expand}}
                                               :acme/strict true})
             ".clj-kondo/hooks/lint.clj" lint-hook
             ".clj-kondo/hooks/xform.clj" transform-hook
             ".clj-kondo/hooks/plainx.clj" plain-transform-hook
             "src/acme.clj" "(ns acme) (defmacro checked [x] x) (defmacro defx [n v] `(def ~n ~v)) (defmacro defp [n v] `(def ~n ~v))"
             "src/app/uses_defp.clj" "(ns app.uses-defp (:require [acme])) (acme/defp z 3)"
             "src/app/uses_checked.clj" "(ns app.uses-checked (:require [acme])) (acme/checked 1)"
             "src/app/uses_defx.clj" "(ns app.uses-defx (:require [acme])) (acme/defx y 2)"
             "src/app/plain.clj" "(ns app.plain) (defn plain [] 1)"}))

(deftest custom-keys-matter-where-hooks-that-read-them-run
  ;; two hooks read :acme/strict: their users are analyzed again (whether a
  ;; hook changes the code can depend on the key, so one that only lints
  ;; counts too). The other transforming hook never reads it.
  (let [root (hooks-fixture)]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      (let [p (sync-project! ix root)]
        (is (contains? (visible-defs (:c ix) p) "app.uses-defx/y") "the transforming hook ran")
        (is (= #{"uses_defx.clj" "uses_checked.clj"}
               (analyzed-files ix root #(set-config! root '{:hooks {:analyze-call {acme/checked hooks.lint/check
                                                                                    acme/defx hooks.xform/expand
                                                                                    acme/defp hooks.plainx/expand}}
                                                            :acme/strict false}))))))))
