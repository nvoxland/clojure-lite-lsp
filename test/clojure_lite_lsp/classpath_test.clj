(ns clojure-lite-lsp.classpath-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu]))

(defn project!
  "A temp project dir containing `files` ({relative-path content})."
  [files]
  (let [root (.getCanonicalFile (tu/temp-dir))]
    (doseq [[path content] files]
      (let [f (io/file root path)]
        (io/make-parents f)
        (if (= :dir content) (.mkdirs f) (spit f content))))
    (str root)))

(deftest reads-clojure-lite-lsp-edn
  (is (= {:aliases [:dev :test] :extra-source-paths []}
         (classpath/project-config (project! {}))))
  (is (= {:aliases [:local] :extra-source-paths ["scripts"]}
         (classpath/project-config (project! {".clojure-lite-lsp.edn" "{:aliases [:local] :extra-source-paths [\"scripts\"]}"})))))

(deftest commands-per-build-tool
  (testing "deps.edn wins, with only the configured aliases it defines"
    (is (= ["clojure" "-Spath" "-A:dev"]
           (classpath/command (project! {"deps.edn" "{:aliases {:dev {} :other {}}}" "bb.edn" "{}"})
                              {:aliases [:dev :test]}))))
  (testing "no matching aliases"
    (is (= ["clojure" "-Spath"]
           (classpath/command (project! {"deps.edn" "{}"}) {:aliases [:dev]}))))
  (testing "Leiningen"
    (is (= ["lein" "with-profile" "+dev,+test" "classpath"]
           (classpath/command (project! {"project.clj" "(defproject x \"1\")"}) {:aliases [:dev :test]}))))
  (testing "babashka"
    (is (= ["bb" "-e" "(println (babashka.classpath/get-classpath))"]
           (classpath/command (project! {"bb.edn" "{}"}) {:aliases []}))))
  (testing "no build file"
    (is (nil? (classpath/command (project! {}) {:aliases []})))))

(deftest classifies-entries
  (let [root (project! {"src/a.clj" "" "dev" :dir "scripts" :dir})
        ext (project! {"lib" :dir})
        jar (str (io/file (project! {"x.jar" "not really a jar"}) "x.jar"))]
    (is (= [{:path (str root "/src") :kind :source-dir :ord 0}
            {:path (str ext "/lib") :kind :external-dir :ord 2}
            {:path jar :kind :jar :ord 3}
            {:path (str root "/dev") :kind :source-dir :ord 0}
            {:path (str root "/scripts") :kind :source-dir :ord 0}]
           (classpath/classify root
                               ["src" "/does/not/exist" (str ext "/lib") jar "src" (str root "/dev")]
                               {:extra-source-paths ["scripts"]})))))

(deftest computes-a-deps-edn-classpath
  (let [root (project! {"deps.edn" "{:paths [\"src\"] :aliases {:dev {:extra-paths [\"dev\"]}}}"
                        "src/a.clj" "(ns a)" "dev/user.clj" "(ns user)"})
        entries (classpath/compute root)]
    (is (= #{(str root "/src") (str root "/dev")}
           (set (map :path (filter #(= :source-dir (:kind %)) entries)))))
    (is (some #(re-find #"clojure-[\d.]+\.jar$" (:path %)) (filter #(= :jar (:kind %)) entries)))))

(deftest memoizes-by-build-files
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src" :dir})
          p (snapshot/ensure-project! c root)
          runs (atom 0)
          run (fn [_cmd _dir] (swap! runs inc) "src")]
      (classpath/memoized! c p root {:run run})
      (classpath/memoized! c p root {:run run})
      (is (= 1 @runs))
      (testing "a changed build file recomputes"
        (spit (io/file root "deps.edn") "{:paths [\"src\" \"more\"]}")
        (classpath/memoized! c p root {:run run})
        (is (= 2 @runs))))))

(deftest a-failing-build-keeps-the-last-classpath
  ;; deps.edn mid-edit, or the build tool offline: indexing goes on with
  ;; what worked last instead of stopping for the whole project
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src" :dir})
          p (snapshot/ensure-project! c root)
          first-cp (classpath/memoized! c p root {:run (fn [_ _] "src")})]
      (spit (io/file root "deps.edn") "{:paths [\"src\"")
      (is (= first-cp (classpath/memoized! c p root {:run (fn [_ _] (throw (ex-info "Error building classpath" {})))})))
      (testing "with nothing that worked before, the usual source dirs"
        (let [other (project! {"deps.edn" "{" "src" :dir})
              entries (classpath/memoized! c (snapshot/ensure-project! c other) other
                                           {:run (fn [_ _] (throw (ex-info "Error" {})))})]
          (is (= [(str other "/src")] (map :path (filter #(= :source-dir (:kind %)) entries)))))))))

(deftest a-local-dependencys-build-file-counts
  ;; a :local/root dep's own deps can change the classpath
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [lib (project! {"deps.edn" "{:paths [\"src\"]}"})
          root (project! {"deps.edn" (pr-str {:paths ["src"] :deps {'acme/lib {:local/root lib}}}) "src" :dir})
          p (snapshot/ensure-project! c root)
          runs (atom 0)
          run (fn [_ _] (swap! runs inc) "src")]
      (classpath/memoized! c p root {:run run})
      (spit (io/file lib "deps.edn") "{:paths [\"src\"] :deps {other/dep {:mvn/version \"1.0\"}}}")
      (classpath/memoized! c p root {:run run})
      (is (= 2 @runs)))))
