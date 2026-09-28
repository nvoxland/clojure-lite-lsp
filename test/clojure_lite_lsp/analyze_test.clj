(ns clojure-lite-lsp.analyze-test
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.kondo-config-test :refer [jar! files-in]]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]))

(defn config [root] (kc/project-config! (tu/temp-dir) root []))

(defn comparable
  "Byte arrays compare by identity; turn them into vectors."
  [x]
  (walk/postwalk #(if (bytes? %) (vec %) %) x))

(defn names-of [kind elements] (set (keep #(when (= kind (:kind %)) (:name %)) elements)))

(deftest analyzes-project-files-into-keyed-units
  (let [root (project! {"src/a.clj" "(ns a) (defn f [] (set/union #{} #{}))"
                        "src/b.cljc" "(ns b) (defn g [] 1)"
                        "src/empty.clj" ""})
        files (mapv #(str root "/src/" %) ["a.clj" "b.cljc" "empty.clj"])
        units (analyze/analyze-files files {:config (config root) :mode :project})]
    (is (= files (map :path units)))
    (testing "unit keys cover content, language, clj-kondo, config and options"
      (let [{:keys [unit-key]} (first units)]
        (is (= "clj" (:lang-key unit-key)))
        (is (= analyze/kondo-version (:kondo-version unit-key)))
        (is (false? (:external? unit-key)))
        (is (every? #(some? (unit-key %)) [:content-hash :config-hash :options-hash])))
      (is (= "cljc" (get-in (second units) [:unit-key :lang-key]))))
    (testing "elements, including unresolved-namespace usages"
      (is (= #{"f"} (names-of :var-def (:elements (first units)))))
      (is (contains? (names-of :var-usage (:elements (first units))) "union")))
    (testing "a file with no analysis is still a unit"
      (is (= [] (:elements (nth units 2)))))))

(deftest sharding-does-not-change-results
  (let [root (project! (into {} (for [i (range 12)]
                                  [(str "src/n" i ".clj") (str "(ns n" i " (:require [n" (mod (inc i) 12) "])) (defn f" i " [] (n" (mod (inc i) 12) "/f" (mod (inc i) 12) "))")])))
        files (vec (for [i (range 12)] (str root "/src/n" i ".clj")))
        cfg (config root)]
    (is (= (comparable (analyze/analyze-files files {:config cfg :mode :project :shards 1}))
           (comparable (analyze/analyze-files files {:config cfg :mode :project :shards 5}))))))

(deftest the-project-config-is-used-and-not-written-to
  (let [root (project! {".clj-kondo/config.edn" "{:lint-as {a/defthing clojure.core/def}}"
                        "src/a.clj" "(ns a) (defmacro defthing [n v] `(def ~n ~v)) (defthing x 1)"
                        "src/b.clj" "(ns b) (defmacro ^{:clj-kondo/lint-as 'clojure.core/let} with-y [bs & body] `(let ~bs ~@body))"})
        cfg (config root)
        units (analyze/analyze-files [(str root "/src/a.clj") (str root "/src/b.clj")] {:config cfg :mode :project})]
    (is (contains? (names-of :var-def (:elements (first units))) "x"))
    (testing "no inline configs are written into the config dir"
      (is (= #{"config.edn"} (files-in (:dir cfg)))))))

(deftest analyzes-jars-by-entry
  (let [j (jar! {"acme/core.clj" "(ns acme.core) (defn pub [] 1) (defn- priv [] 2)"
                 "acme/util.cljc" "(ns acme.util) (defn u [] 1)"
                 "clj-kondo.exports/acme/lib/config.edn" "{}"})
        [{:keys [jar entries]}] (analyze/analyze-jars [j] {:configs {j (kc/neutral-config! (tu/temp-dir))}
                                                           :jar-hashes {j (byte-array [7])}})
        by-path (into {} (map (juxt :entry-path identity)) entries)]
    (is (= j jar))
    (is (= #{"acme/core.clj" "acme/util.cljc"} (set (keys by-path))))
    (testing "dependency analysis: public definitions only"
      (is (= #{"pub"} (names-of :var-def (:elements (by-path "acme/core.clj"))))))
    (testing "entries are keyed by their own content"
      (is (true? (get-in by-path ["acme/core.clj" :unit-key :external?])))
      (is (not= (vec (get-in by-path ["acme/core.clj" :unit-key :content-hash]))
                (vec (get-in by-path ["acme/util.cljc" :unit-key :content-hash])))))))
