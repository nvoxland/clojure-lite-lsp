(ns clojure-lite-lsp.kondo-config-test
  (:require
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.test-util :as tu :refer [project! jar! maven-jar! files-in]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest reads-exports-from-jars-and-dirs
  (let [j (jar! {"clj-kondo.exports/acme/lib/config.edn" "{:lint-as {acme.lib/defthing clojure.core/def}}"
                 "clj-kondo.exports/acme/lib/hooks/h.clj" "(ns hooks.h)"
                 "acme/lib.clj" "(ns acme.lib)"})
        d (project! {"clj-kondo.exports/other/lib/config.edn" "{}"})]
    (is (= #{"acme/lib/config.edn" "acme/lib/hooks/h.clj"} (set (keys (kc/exports j)))))
    (is (= #{"other/lib/config.edn"} (set (keys (kc/exports d)))))
    (is (= {} (kc/exports (jar! {"a.clj" "(ns a)"}))))))

(deftest maven-dependencies-come-from-poms
  (let [a (maven-jar! "acme" "a" [["acme" "b"]] {})
        b (maven-jar! "acme" "b" [["acme" "c"] ["not" "on-classpath"]] {})
        c (maven-jar! "acme" "c" [] {})
        plain (jar! {"x.clj" "(ns x)"})]
    (is (= ["acme" "a"] (kc/maven-coords a)))
    (is (nil? (kc/maven-coords plain)))
    (testing "the transitive closure, among the classpath's jars"
      (is (= {a #{b c} b #{c} c #{} plain #{}}
             (kc/dependency-closure [a b c plain]))))))

(deftest materialized-configs-are-content-addressed
  (let [cache (tu/temp-dir)
        m1 (kc/materialize! cache {"config.edn" (.getBytes "{}") "acme/lib/config.edn" (.getBytes "{:x 1}")})
        m2 (kc/materialize! cache {"acme/lib/config.edn" (.getBytes "{:x 1}") "config.edn" (.getBytes "{}")})
        m3 (kc/materialize! cache {"config.edn" (.getBytes "{:changed true}")})]
    (is (= (:dir m1) (:dir m2)))
    (is (= (vec (:hash m1)) (vec (:hash m2))))
    (is (not= (:dir m1) (:dir m3)))
    (is (= #{"config.edn" "acme/lib/config.edn"} (files-in (:dir m1))))))

(deftest project-config-is-a-private-copy-plus-classpath-exports
  (let [cache (tu/temp-dir)
        root (project! {".clj-kondo/config.edn" "{:lint-as {a/b clojure.core/def}}"
                        ".clj-kondo/hooks/mine.clj" "(ns hooks.mine)"
                        ".clj-kondo/.cache/v1/clj/a.transit.json" "cache"
                        ".clj-kondo/inline-configs/a.clj/config.edn" "{}"
                        ".clj-kondo/gen-macros/a.clj" "x"})
        j (jar! {"clj-kondo.exports/acme/lib/config.edn" "{}"})
        {:keys [dir]} (kc/project-config! cache root [{:path j :kind :jar :ord 1}])]
    (testing "clj-kondo's caches and generated files are left out"
      (is (= #{"config.edn" "hooks/mine.clj" "imports/acme/lib/config.edn"} (files-in dir))))
    (testing "the project's own directory is untouched"
      (is (.exists (io/file root ".clj-kondo/.cache/v1/clj/a.transit.json")))
      (is (not (.exists (io/file root ".clj-kondo/imports")))))))

(deftest a-project-without-a-kondo-config-has-one
  (let [{:keys [dir]} (kc/project-config! (tu/temp-dir) (project! {}) [])]
    (is (= #{} (files-in dir)))))

(deftest jar-configs-include-dependency-exports
  (let [cache (tu/temp-dir)
        ham (maven-jar! "acme" "ham" [] {"clj-kondo.exports/acme/ham/config.edn" "{:hooks {}}"})
        dtype (maven-jar! "acme" "dtype" [["acme" "ham"]] {"dtype.clj" "(ns dtype)"})
        plain (jar! {"plain.clj" "(ns plain)"})
        ctx (kc/jar-context [ham dtype plain])]
    (testing "a jar sees its own and its dependencies' exports"
      (is (= #{"imports/acme/ham/config.edn"} (files-in (:dir (kc/jar-config! cache ctx dtype))))))
    (testing "jars with no exports anywhere share the neutral config"
      (let [n1 (kc/jar-config! cache ctx plain)]
        (is (= #{} (files-in (:dir n1))))
        (is (= (vec (:hash n1)) (vec (:hash (kc/neutral-config! cache)))))))))

(deftest copied-dependency-configs-and-bookkeeping-do-not-change-the-project-config
  ;; a main checkout has what clj-kondo copied in (dependency configs at
  ;; <org>/<lib>/, bookkeeping files); a fresh worktree of the same branch
  ;; has only what git tracks. Their project configs must be the same, or
  ;; every file of the new worktree is analyzed again.
  (let [j (jar! {"clj-kondo.exports/acme/lib/config.edn" "{:lint-as {acme.lib/defthing clojure.core/def}}"
                 "clj-kondo.exports/acme/lib/hooks/h.clj" "(ns hooks.h)"})
        entries [{:path j :kind :jar :ord 1}]
        tracked {".clj-kondo/config.edn" "{:linters {:unused-binding {:level :off}}}"
                 ".clj-kondo/hooks/mine.clj" "(ns hooks.mine)"}
        fresh (project! tracked)
        main (project! (merge tracked
                              {".clj-kondo/acme/lib/config.edn" "{:lint-as {acme.lib/defthing clojure.core/def}}"
                               ".clj-kondo/acme/lib/hooks/h.clj" "(ns hooks.h)"
                               ".clj-kondo/imports/acme/lib/config.edn" "{:an-older copy}"
                               ".clj-kondo/.lock" ""
                               ".clj-kondo/.deps.edn.md5sum" "d41d8cd98f00b204e9800998ecf8427e"}))
        cache (tu/temp-dir)]
    (is (= (vec (:hash (kc/project-config! cache fresh entries)))
           (vec (:hash (kc/project-config! cache main entries)))))
    (testing "project-specific config still counts"
      (let [custom (project! (merge tracked {".clj-kondo/myorg/custom/config.edn" "{:lint-as {my/m clojure.core/let}}"}))]
        (is (not= (vec (:hash (kc/project-config! cache fresh entries)))
                  (vec (:hash (kc/project-config! cache custom entries)))))))
    (testing "and so does the project's own config.edn"
      (let [changed (project! (assoc tracked ".clj-kondo/config.edn" "{}"))]
        (is (not= (vec (:hash (kc/project-config! cache fresh entries)))
                  (vec (:hash (kc/project-config! cache changed entries)))))))))

(deftest exports-are-found-below-the-top
  ;; like clj-kondo: a clj-kondo.exports segment anywhere (malli ships its
  ;; config at resources/clj-kondo/clj-kondo.exports/metosin/malli/)
  (let [d (project! {"clj-kondo/clj-kondo.exports/metosin/malli/config.edn" "{:lint-as {}}"})
        j (jar! {"resources/clj-kondo.exports/acme/lib/config.edn" "{}"})]
    (is (= #{"metosin/malli/config.edn"} (set (keys (kc/exports d)))))
    (is (= #{"acme/lib/config.edn"} (set (keys (kc/exports j)))))))
