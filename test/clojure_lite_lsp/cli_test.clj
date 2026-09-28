(ns clojure-lite-lsp.cli-test
  "The commands for humans: index projects and wait, collect garbage."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.daemon-test :refer [home project! fast client-db]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn opts
  "Client options whose daemon runs in this process."
  [h daemons]
  {:home h :spawn! #(swap! daemons conj (future (daemon/serve! (merge fast {:home h :version version/version}))))})

(defn stop! [h daemons]
  (with-open [c (client-db h)] (client/request-stop! c))
  (doseq [d @daemons] (deref d 30000 :timeout)))

(defn defs [h]
  (with-open [c (client-db h)]
    (db/query-value c "SELECT count(*) FROM definition WHERE kind = 1")))

(deftest index-waits-until-the-projects-are-indexed
  (let [h (home)
        daemons (atom [])
        a (project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        b (project! {"src/b.clj" "(ns b) (defn g [] 1) (defn h [] 2)"})
        progress (atom [])]
    (try
      (let [result (cli/index! (opts h daemons) [a b] #(swap! progress conj %))]
        (is (= 3 (defs h)) "done when it returns")
        (is (= {a 1 b 1} (:files result)))
        (is (seq @progress) "reports progress"))
      (finally (stop! h daemons)))))

(deftest gc-goes-through-the-daemon
  (let [h (home)
        daemons (atom [])
        root (project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        o (opts h daemons)]
    (try
      (cli/index! o [root] (fn [_]))
      (spit (io/file root "src/a.clj") "(ns a) (defn f [] 2)")
      (cli/index! o [root] (fn [_]))
      (testing "the replaced analysis is dropped"
        (is (= 1 (:units (cli/gc! o)))))
      (is (= 1 (count @daemons)) "by the one daemon")
      (finally (stop! h daemons)))))

(deftest concurrent-gc-requests-all-finish
  (let [h (home)
        daemons (atom [])
        o (opts h daemons)]
    (try
      (cli/index! o [(project! {"src/a.clj" "(ns a)"})] (fn [_]))
      (let [runs (doall (for [_ (range 3)] (future (cli/gc! o))))]
        (is (every? map? (map #(deref % 60000 :hung) runs))))
      (finally (stop! h daemons)))))

(deftest index-refuses-what-isnt-a-directory
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not a directory"
                        (cli/index! {:home (home) :spawn! #(throw (ex-info "no daemon needed" {}))}
                                    ["/no/such/dir"] (fn [_])))))

(deftest query-answers-from-the-projects-index
  (let [h (home)
        daemons (atom [])
        root (project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})
        o (opts h daemons)
        q #(cli/query! o %& {:cwd root})]
    (try
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
          (is (str/includes? out "definition"))))
      (finally (stop! h daemons)))))

(deftest query-outside-a-project-and-failures
  (let [h (home)
        daemons (atom [])
        o (opts h daemons)
        bare (str (tu/temp-dir))]
    (try
      (testing "not in a Clojure project: said so, exit 1"
        (let [{:keys [exit out]} (cli/query! o ["definition" "a/b"] {:cwd bare})]
          (is (= 1 exit))
          (is (str/includes? out "Clojure project"))))
      (testing "a failure that isn't a usage error: exit 1, without the usage text"
        (let [{:keys [exit out]} (cli/query! o ["definition" "a/b" "--project" "/no/such/dir"] {:cwd bare})]
          (is (= 1 exit))
          (is (not (str/includes? out "Commands:")))))
      (testing "--limit"
        (let [root (project! {"src/app/a.clj" "(ns app.a)\n(defn g [] 1)\n(g) (g) (g)\n"})
              {:keys [out]} (cli/query! o ["references" "app.a/g" "--limit" "1"] {:cwd root})]
          (is (= 2 (count (str/split-lines out))))))
      (finally (stop! h daemons)))))

(deftest stop-ends-the-indexer
  (let [h (home)
        daemons (atom [])
        o (opts h daemons)]
    (is (= :not-running (cli/stop! o)))
    (cli/index! o [(project! {"src/a.clj" "(ns a)"})] (fn [_]))
    (is (= :stopped (cli/stop! o)))
    (is (not (lock/held? (:daemon-lock (home/paths h)))))
    (doseq [d @daemons] (deref d 30000 :timeout))))
