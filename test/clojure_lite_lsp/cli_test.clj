(ns clojure-lite-lsp.cli-test
  "The commands besides the servers: index projects and wait, collect
  garbage, stop the indexer, query."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.daemon-fixture :refer [home client-db with-in-process-daemons]]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.index-fixture :as index-fixture]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.test-util :as tu :refer [build-free-project!]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- defs
  "How many var definitions the index of home `h` has."
  [h]
  (with-open [c (client-db h)]
    (index-fixture/count-of c "definition WHERE kind = ?" (kinds/code :var-def))))

(def ^:private ignore-progress (constantly nil))

(deftest index-waits-until-the-projects-are-indexed
  (with-in-process-daemons [o h _]
    (let [a (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})
          b (build-free-project! {"src/b.clj" "(ns b) (defn g [] 1) (defn h [] 2)"})
          progress (atom [])
          result (cli/index! o [a b] #(swap! progress conj %))]
      (is (= 3 (defs h)) "done when it returns")
      (is (= {a 1 b 1} (:files result)))
      (is (seq @progress) "reports progress"))))

(deftest gc-goes-through-the-daemon
  (with-in-process-daemons [o _ daemons]
    (let [root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})]
      (cli/index! o [root] ignore-progress)
      (spit (io/file root "src/a.clj") "(ns a) (defn f [] 2)")
      (cli/index! o [root] ignore-progress)
      (testing "the replaced analysis is dropped"
        (is (= 1 (:units (cli/gc! o)))))
      (is (= 1 (count @daemons)) "by the one daemon"))))

(deftest concurrent-gc-requests-all-finish
  (with-in-process-daemons [o _ _]
    (cli/index! o [(build-free-project! {"src/a.clj" "(ns a)"})] ignore-progress)
    (let [runs (doall (for [_ (range 3)] (future (cli/gc! o))))]
      (is (every? map? (map #(deref % 60000 :hung) runs))))))

(deftest index-refuses-what-isnt-a-directory
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not a directory"
                        (cli/index! {:home (home) :spawn! #(throw (ex-info "no daemon needed" {}))}
                                    ["/no/such/dir"] ignore-progress))))

(deftest query-answers-from-the-projects-index
  (with-in-process-daemons [o _ _]
    (let [root (build-free-project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n"
                                     "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})
          q #(cli/query! o %& {:cwd root})]
      (testing "it indexes the project first, then answers"
        (is (= {:exit 0 :out "src/app/b.clj:2:2: (a/greet 1)"} (q "references" "app.a/greet"))))
      (testing "the project is found from a directory inside it"
        (is (= 0 (:exit (cli/query! o ["definition" "app.a/greet"] {:cwd (str root "/src/app")})))))
      (testing "positions are relative to the current directory"
        (is (= "src/app/a.clj:2:7: (defn greet [who] who)"
               (:out (cli/query! o ["definition" "b.clj:2:4"] {:cwd (str root "/src/app")})))))
      (testing "--json"
        (is (= [{:path (str root "/src/app/b.clj") :line 2 :column 2 :end-line 2 :end-column 9}]
               (:results (json/parse-string (:out (q "references" "app.a/greet" "--json")) true)))))
      (testing "--no-sync answers from the index as it is"
        (is (= 0 (:exit (q "definition" "app.a/greet" "--no-sync")))))
      (testing "no command: what there is"
        (let [{:keys [exit out]} (q)]
          (is (= 0 exit))
          (is (str/includes? out "references"))))
      (testing "an unknown command: an error and the commands"
        (let [{:keys [exit out]} (q "frobnicate" "x")]
          (is (= 2 exit))
          (is (str/includes? out "definition")))))))

(deftest query-outside-a-project-and-failures
  (with-in-process-daemons [o _ _]
    (let [bare (str (tu/temp-dir))]
      (testing "not in a Clojure project: said so, exit 1"
        (let [{:keys [exit out]} (cli/query! o ["definition" "a/b"] {:cwd bare})]
          (is (= 1 exit))
          (is (str/includes? out "Clojure project"))))
      (testing "a failure that isn't a usage error: exit 1, without the usage text"
        (let [{:keys [exit out]} (cli/query! o ["definition" "a/b" "--project" "/no/such/dir"] {:cwd bare})]
          (is (= 1 exit))
          (is (not (str/includes? out "Commands:")))))
      (testing "--limit"
        (let [root (build-free-project! {"src/app/a.clj" "(ns app.a)\n(defn g [] 1)\n(g) (g) (g)\n"})
              {:keys [out]} (cli/query! o ["references" "app.a/g" "--limit" "1"] {:cwd root})]
          (is (= 2 (count (str/split-lines out)))))))))

(deftest stop-ends-the-daemon
  (with-in-process-daemons [o h _]
    (is (= :not-running (cli/stop! o)))
    (cli/index! o [(build-free-project! {"src/a.clj" "(ns a)"})] ignore-progress)
    (is (= :stopped (cli/stop! o)))
    (is (not (lock/held? (:daemon-lock (home/paths h)))))))
