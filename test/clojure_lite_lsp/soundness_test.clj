(ns clojure-lite-lsp.soundness-test
  "Ways wrong analysis could be stored under a key that says it's right,
  and then be shared by every project with that key."
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.index-fixture :refer [analyzed-files sync-project! temp-indexer visible-defs]]
   [clojure-lite-lsp.test-util :as tu :refer [project!]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(defn hook [suffix]
  (str "(ns hooks.named (:require [clj-kondo.hooks-api :as api]))
        (defn named [{:keys [node]}]
          (let [[_ n] (:children node)]
            {:node (api/list-node [(api/token-node 'def) (api/token-node (symbol (str (:value n) \"" suffix "\"))) (api/token-node 1)])}))"))

(defn hook-project []
  (project! {"deps.edn" "{:paths [\"src\"]}"
             ".clj-kondo/config.edn" "{:hooks {:analyze-call {acme/named hooks.named/named}}}"
             ".clj-kondo/hooks/named.clj" (hook "-one")
             "src/acme.clj" "(ns acme) (defmacro named [n])"
             "src/app/uses.clj" "(ns app.uses (:require [acme])) (acme/named x)"}))

(deftest a-changed-hook-runs-its-new-code
  ;; clj-kondo loads hook code once per process and reloads only a file it
  ;; saw change; every config is its own directory, so it never sees one
  (let [root (hook-project)]
    (with-open [ix (temp-indexer)]
      (let [p (sync-project! ix root)]
        (is (contains? (visible-defs (:c ix) p) "app.uses/x-one"))
        (spit (io/file root ".clj-kondo/hooks/named.clj") (hook "-two"))
        (sync-project! ix root)
        (is (contains? (visible-defs (:c ix) p) "app.uses/x-two"))
        (is (not (contains? (visible-defs (:c ix) p) "app.uses/x-one")))))))

(deftest analyses-never-overlap
  ;; hooks share clj-kondo's process-wide state, so runs (of possibly
  ;; different configs) must not overlap, pipelined or not
  (let [root (project! (into {"deps.edn" "{:paths [\"src\"]}"}
                             (for [i (range 6)] [(str "src/app/n" i ".clj") (str "(ns app.n" i ")")])))
        active (atom 0)
        most (atom 0)
        real analyze/analyze-files]
    (with-open [ix (temp-indexer :batch-sizes {:file 1})]
      (with-redefs [analyze/analyze-files (fn [paths opts]
                                            (swap! most max (swap! active inc))
                                            (try (Thread/sleep 50) (real paths opts)
                                                 (finally (swap! active dec))))]
        (sync-project! ix root)))
    (is (= 1 @most))))

(deftest a-file-changed-while-analyzed-is-analyzed-again
  ;; the key must hash the content that was analyzed
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a) (defn v1 [])"})
        f (io/file root "src/app/a.clj")
        changed? (atom false)
        real @#'analyze/run-kondo]
    (with-open [ix (temp-indexer)]
      (with-redefs [analyze/run-kondo (fn [& args]
                                        (let [r (apply real args)]
                                          ;; saved again while clj-kondo was busy
                                          (when (compare-and-set! changed? false true)
                                            (spit f "(ns app.a) (defn v2 [])"))
                                          r))]
        (let [p (sync-project! ix root)]
          (is (contains? (visible-defs (:c ix) p) "app.a/v2"))
          (is (not (contains? (visible-defs (:c ix) p) "app.a/v1"))))))))

(deftest only-the-projects-config-is-used
  ;; clj-kondo would merge ~/.config/clj-kondo, which no key covers
  (let [opts (atom nil)
        real kondo/run!]
    (with-redefs [kondo/run! (fn [o] (reset! opts o) (real o))]
      (analyze/analyze-files [(str (project! {"a.clj" "(ns a)"}) "/a.clj")]
                             {:config {:dir (str (tu/temp-dir)) :hash (byte-array 1)} :mode :project}))
    (is (true? (:repro @opts)))))

(deftest external-dirs-reuse-only-exact-analyses
  ;; external dirs are analyzed without usages, so they record no
  ;; references: nothing shows which config changes they're safe from
  (let [ext (project! {"deps.edn" "{:paths [\"src\"]}"
                       "src/clj-kondo.exports/ext/m/config.edn" "{:lint-as {ext.m/defthing clojure.core/def}}"
                       "src/ext/m.clj" "(ns ext.m) (defmacro defthing [n v])"
                       "src/ext/uses.clj" "(ns ext.uses (:require [ext.m])) (ext.m/defthing thing 1)"})
        root (project! {"deps.edn" (pr-str {:paths ["src"] :deps {'ext/core {:local/root ext}}})
                        "src/app/a.clj" "(ns app.a)"})]
    (with-open [ix (temp-indexer)]
      (sync-project! ix root)
      (is (contains? (analyzed-files ix root #(spit (io/file ext "src/clj-kondo.exports/ext/m/config.edn")
                                                    "{:lint-as {ext.m/defthing clojure.core/defonce}}"))
                     "uses.clj")))))
