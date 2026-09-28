(ns clojure-lite-lsp.config-sig-test
  (:require
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.config-sig :as sig]
   [clojure.test :refer [deftest is testing]]))

(def hooks-a "(ns hooks.a (:require [clj-kondo.hooks-api :as api])) (defn m [{:keys [node]}] {:node node})")
(def hooks-b "(ns hooks.b (:require [clj-kondo.hooks-api :as api])) (defn m [{:keys [node]}] {:node node})")

(def base-config
  {"config.edn" (pr-str '{:lint-as {acme/defthing clojure.core/def acme/letish clojure.core/let}
                          :hooks {:analyze-call {acme/one hooks.a/m acme/two hooks.a/m acme/three hooks.b/m}}
                          :linters {:unused-binding {:level :off}}})
   "hooks/a.clj" hooks-a
   "hooks/b.clj" hooks-b})

(defn config-dir
  "A config dir of `files` ({path content}, relative to the dir)."
  [files]
  (str (project! (into {} (map (fn [[p c]] [(str "cfg/" p) c])) files)) "/cfg"))

(defn diff [a b]
  (dissoc (sig/diff (sig/signature (config-dir a)) (sig/signature (config-dir b))) :custom-readers))

(defn with-config [f] (update base-config "config.edn" #(pr-str (f (read-string %)))))

(deftest identical-configs
  (is (= {:global-same? true :custom-changed #{} :changed #{}} (diff base-config base-config))))

(deftest a-lint-as-change-is-that-symbol
  (is (= {:global-same? true :custom-changed #{} :changed #{"acme/defthing"}}
         (diff base-config (with-config #(assoc-in % [:lint-as 'acme/defthing] 'clojure.core/defn))))))

(deftest linter-settings-do-not-matter
  (is (= {:global-same? true :custom-changed #{} :changed #{}}
         (diff base-config (with-config #(assoc-in % [:linters :unused-binding :level] :warning))))))

(deftest a-hook-code-change-is-every-symbol-it-handles
  (is (= {:global-same? true :custom-changed #{} :changed #{"acme/one" "acme/two"}}
         (diff base-config (assoc base-config "hooks/a.clj" (str hooks-a "\n(defn helper [])"))))))

(deftest config-in-ns-is-per-namespace
  (testing "linters only: nothing"
    (is (= {:global-same? true :custom-changed #{} :changed #{}}
           (diff base-config (with-config #(assoc-in % [:config-in-ns 'app.core :linters :unresolved-symbol :level] :off))))))
  (testing "analysis settings: that namespace"
    (is (= {:global-same? true :custom-changed #{} :changed #{"ns:app.core"}}
           (diff base-config (with-config #(assoc-in % [:config-in-ns 'app.core :lint-as] '{acme/x clojure.core/def})))))))

(deftest ns-groups-matter-only-with-analysis-settings
  (let [group [{:pattern "app\\..*" :name 'app-group}]]
    (testing "a group only linters are configured for: nothing"
      (is (:global-same? (diff base-config (with-config #(assoc % :ns-groups group
                                                                :config-in-ns {'app-group {:linters {:unresolved-symbol {:level :off}}}}))))))
    (testing "a group with analysis settings: global"
      (let [with-group #(with-config (fn [c] (assoc c :ns-groups [{:pattern % :name 'app-group}]
                                                    :config-in-ns {'app-group {:lint-as '{acme/x clojure.core/def}}})))]
        (is (false? (:global-same? (diff (with-group "app\\..*") (with-group "other\\..*")))))))))

(deftest exported-configs-count-too
  (let [with-import #(assoc base-config "imports/acme/lib/config.edn" (pr-str {:lint-as {'acme.lib/deft %}}))]
    (is (= {:global-same? true :custom-changed #{} :changed #{"acme.lib/deft"}}
           (diff (with-import 'clojure.core/def) (with-import 'clojure.core/defn))))))

(deftest custom-keys-are-their-own-part
  ;; top-level keys clj-kondo doesn't know (:metabase/modules) are read only
  ;; by hooks: kept apart from the global part
  (let [d (diff (with-config #(assoc % :acme/modules {:a 1})) (with-config #(assoc % :acme/modules {:a 2})))]
    (is (:global-same? d))
    (is (= #{:acme/modules} (:custom-changed d)))
    (is (= #{} (:changed d)))))

(deftest hooks-say-which-custom-keys-they-read
  (let [reads "(ns hooks.a) (defn m [{:keys [node config]}] (get config :acme/modules) {:node node})"
        s (sig/signature (config-dir (-> (assoc base-config "hooks/a.clj" reads)
                                         (update "config.edn" #(pr-str (assoc (read-string %) :acme/modules {:a 1}))))))]
    (is (= #{:acme/modules} (get-in s [:mentions "acme/one"])))
    (is (empty? (get-in s [:mentions "acme/three"])))))

(deftest a-custom-key-change-is-the-macros-whose-hooks-read-it
  (let [reads "(ns hooks.a) (defn m [{:keys [node config]}] (get config :acme/modules) {:node node})"
        cfg #(-> (assoc base-config "hooks/a.clj" reads)
                 (update "config.edn" (fn [c] (pr-str (assoc (read-string c) :acme/modules %)))))]
    (is (= #{"acme/one" "acme/two"}
           (:custom-readers (sig/diff (sig/signature (config-dir (cfg {:a 1}))) (sig/signature (config-dir (cfg {:a 2})))))))))

(deftest hooks-on-an-ns-group-are-global
  ;; a hook keyed on a group (my-group/deftest) applies to whatever
  ;; namespaces match it: no file references it by that name
  (let [with-group #(with-config (fn [c] (assoc c :ns-groups [{:pattern "app\\..*" :name 'app-group}]
                                                :hooks {:analyze-call {'app-group/deft %}})))]
    (is (false? (:global-same? (diff (with-group 'hooks.a/m) (with-group 'hooks.b/m)))))))

(deftest hook-code-follows-every-kind-of-require
  (let [helper "(ns hooks.util) (defn h [])"
        cfg (fn [a-code util-code]
              (-> base-config
                  (assoc "hooks/a.clj" a-code "hooks/util.clj" util-code "hooks/other.clj" "(ns hooks.other)")))]
    (doseq [[how a-code] [["a prefix list" "(ns hooks.a (:require [hooks [util :as u] other])) (defn m [{:keys [node]}] {:node node})"]
                          ["a top-level require" "(ns hooks.a) (require '[hooks.util :as u]) (defn m [{:keys [node]}] {:node node})"]
                          ["a :use" "(ns hooks.a (:use hooks.util)) (defn m [{:keys [node]}] {:node node})"]]]
      (testing how
        (is (= #{"acme/one" "acme/two"}
               (:changed (diff (cfg a-code helper) (cfg a-code (str helper " (defn h2 [])"))))))))))

(deftest a-hook-namespace-in-two-places-counts-both
  ;; which one clj-kondo loads isn't clojure-lite-lsp's to guess: a change to either
  ;; counts
  (let [both (fn [root imported] (assoc base-config "hooks/a.clj" root "imports/acme/lib/hooks/a.clj" imported))
        changed (str hooks-a " (defn x [])")]
    (is (= #{"acme/one" "acme/two"} (:changed (diff (both hooks-a hooks-a) (both hooks-a changed)))))
    (is (= #{"acme/one" "acme/two"} (:changed (diff (both hooks-a hooks-a) (both changed hooks-a)))))))

(deftest hook-code-with-auto-resolved-keywords
  ;; ::alias/kw can't be read without the hook's aliases; its requires must
  ;; still be followed
  (let [a-code "(ns hooks.a (:require [clj-kondo.hooks-api :as api] [hooks.util :as u])) (def k ::api/x) (defn m [{:keys [node]}] {:node node})"
        cfg #(assoc base-config "hooks/a.clj" a-code "hooks/util.clj" %)]
    (is (= #{"acme/one" "acme/two"}
           (:changed (diff (cfg "(ns hooks.util)") (cfg "(ns hooks.util) (defn h [])")))))))

(deftest the-one-linter-analysis-uses-counts
  ;; csl keeps :unresolved-namespace findings: its settings change results
  (is (false? (:global-same? (diff base-config
                                   (with-config #(assoc-in % [:linters :unresolved-namespace :exclude] '[foo])))))))
