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

(deftest ns-groups-are-global
  (is (false? (:global-same? (diff base-config (with-config #(assoc % :ns-groups [{:pattern "app\\..*" :name 'app-group}])))))))

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
  (let [reads (str "(ns hooks.a) (defn m [{:keys [node config]}] (get config :acme/modules) {:node node})")
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
