(ns clojure-lite-lsp.cli-test
  "The commands for humans: index projects and wait, collect garbage."
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.daemon-test :refer [home project! fast client-db]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.version :as version]))

(defn opts
  "Client options whose daemon runs in this process."
  [h daemons]
  {:home h :spawn! #(swap! daemons conj (future (daemon/run! (merge fast {:home h :version version/version}))))})

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
