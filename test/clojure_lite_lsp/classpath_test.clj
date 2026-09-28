(ns clojure-lite-lsp.classpath-test
  (:require
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu :refer [project!]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest reads-clojure-lite-lsp-edn
  (is (= {:aliases [:dev :test] :extra-source-paths []}
         (classpath/project-config (project! {}))))
  (is (= {:aliases [:local] :extra-source-paths ["scripts"]}
         (classpath/project-config (project! {".clojure-lite-lsp.edn" "{:aliases [:local] :extra-source-paths [\"scripts\"]}"})))))

(def bb-command ["bb" "-e" "(println (babashka.classpath/get-classpath))"])

(deftest a-command-per-build-tool
  (testing "deps.edn, with only the configured aliases it defines"
    (is (= [["clojure" "-Spath" "-A:dev"]]
           (classpath/commands (project! {"deps.edn" "{:aliases {:dev {} :other {}}}"})
                               {:aliases [:dev :test]}))))
  (testing "no matching aliases"
    (is (= [["clojure" "-Spath"]]
           (classpath/commands (project! {"deps.edn" "{}"}) {:aliases [:dev]}))))
  (testing "Leiningen"
    (is (= [["lein" "with-profile" "+dev,+test" "classpath"]]
           (classpath/commands (project! {"project.clj" "(defproject x \"1\")"}) {:aliases [:dev :test]}))))
  (testing "babashka"
    (is (= [bb-command] (classpath/commands (project! {"bb.edn" "{}"}) {:aliases []}))))
  (testing "babashka's tasks beside a build tool: both"
    (is (= [["clojure" "-Spath"] bb-command]
           (classpath/commands (project! {"deps.edn" "{}" "bb.edn" "{}"}) {:aliases []}))))
  (testing "no build file"
    (is (= [] (classpath/commands (project! {}) {:aliases []})))))

(deftest deps-edn-and-bb-edn-both-give-source-dirs
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [root (project! {"deps.edn" "{}" "bb.edn" "{}" "src" :dir "bb" :dir})
          run (fn [cmd _dir] (if (= "bb" (first cmd)) "bb" "src"))
          entries (classpath/memoized! c (snapshot/ensure-project! c root) root {:run run})]
      (is (= #{(str root "/src") (str root "/bb")}
             (set (map :path (filter #(= :source-dir (:kind %)) entries))))))))

(deftest a-folder-without-a-build-file
  ;; its src and test, as far as they exist
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [root (project! {"src" :dir "test" :dir "other" :dir})
          p (snapshot/ensure-project! c root)
          entries (classpath/memoized! c p root {:run (fn [& _] (throw (ex-info "no build tool to run" {})))})]
      (is (= #{(str root "/src") (str root "/test")} (set (map :path entries))))
      (is (nil? (classpath/error c p)))
      (let [only-src (project! {"src" :dir})]
        (is (= [(str only-src "/src")] (map :path (classpath/compute only-src))))))))

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
