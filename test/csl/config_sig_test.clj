(ns csl.config-sig-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.classpath-test :refer [project!]]
   [csl.config-sig :as sig]))

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

(defn diff [a b] (sig/diff (sig/signature (config-dir a)) (sig/signature (config-dir b))))

(defn with-config [f] (update base-config "config.edn" #(pr-str (f (read-string %)))))

(deftest identical-configs
  (is (= {:global-same? true :changed #{}} (diff base-config base-config))))

(deftest a-lint-as-change-is-that-symbol
  (is (= {:global-same? true :changed #{"acme/defthing"}}
         (diff base-config (with-config #(assoc-in % [:lint-as 'acme/defthing] 'clojure.core/defn))))))

(deftest linter-settings-do-not-matter
  (is (= {:global-same? true :changed #{}}
         (diff base-config (with-config #(assoc-in % [:linters :unused-binding :level] :warning))))))

(deftest a-hook-code-change-is-every-symbol-it-handles
  (is (= {:global-same? true :changed #{"acme/one" "acme/two"}}
         (diff base-config (assoc base-config "hooks/a.clj" (str hooks-a "\n(defn helper [])"))))))

(deftest config-in-ns-is-per-namespace
  (testing "linters only: nothing"
    (is (= {:global-same? true :changed #{}}
           (diff base-config (with-config #(assoc-in % [:config-in-ns 'app.core :linters :unresolved-symbol :level] :off))))))
  (testing "analysis settings: that namespace"
    (is (= {:global-same? true :changed #{"ns:app.core"}}
           (diff base-config (with-config #(assoc-in % [:config-in-ns 'app.core :lint-as] '{acme/x clojure.core/def})))))))

(deftest ns-groups-are-global
  (is (false? (:global-same? (diff base-config (with-config #(assoc % :ns-groups [{:pattern "app\\..*" :name 'app-group}])))))))

(deftest exported-configs-count-too
  (let [with-import #(assoc base-config "imports/acme/lib/config.edn" (pr-str {:lint-as {'acme.lib/deft %}}))]
    (is (= {:global-same? true :changed #{"acme.lib/deft"}}
           (diff (with-import 'clojure.core/def) (with-import 'clojure.core/defn))))))
