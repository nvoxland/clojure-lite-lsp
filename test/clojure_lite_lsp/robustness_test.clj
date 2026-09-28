(ns clojure-lite-lsp.robustness-test
  "The indexer keeps going, and keeps its results right, when things go
  wrong around it."
  (:require
   [clj-kondo.impl.hooks :as hooks]
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon-test :refer [home]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.gc :as gc]
   [clojure-lite-lsp.gc-test :as gc-test]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.indexer-test :refer [visible-defs sync-project!]]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.kondo-hooks :as kondo-hooks]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(deftest hooks-load-one-at-a-time
  ;; the concurrent clj-kondo runs share one hook interpreter: loading a
  ;; hook's code in two at once can lose it
  (let [hook "(ns hooks.named (:require [clj-kondo.hooks-api :as api])) (defn named [{:keys [node]}] {:node (api/list-node [(api/token-node 'def) (api/token-node 'gen) (api/token-node 1)])})"
        root (project! (into {".clj-kondo/config.edn" "{:hooks {:analyze-call {acme/named hooks.named/named}}}"
                              ".clj-kondo/hooks/named.clj" hook
                              "src/acme.clj" "(ns acme) (defmacro named [])"}
                             (for [i (range 16)] [(str "src/app/f" i ".clj") (str "(ns app.f" i " (:require [acme])) (acme/named)")])))
        cfg (kc/project-config! (tu/temp-dir) root [])
        active (atom 0)
        most (atom 0)
        real @#'hooks/hook-fn*]
    (with-redefs [hooks/hook-fn* (fn [& args]
                                   (swap! most max (swap! active inc))
                                   (try (Thread/sleep 20) (apply real args)
                                        (finally (swap! active dec))))]
      (reset! @#'kondo-hooks/hooks-config nil)
      (analyze/analyze-files (vec (for [i (range 16)] (str root "/src/app/f" i ".clj")))
                             {:config cfg :mode :project :shards 8}))
    (is (= 1 @most))))

(deftest a-hook-is-looked-up-once-per-config
  ;; clj-kondo empties its hook cache whenever it sees another config
  ;; object, and each concurrent run has its own: the lookups (loading the
  ;; code, hashing its file) would repeat at nearly every hooked call
  (let [hook "(ns hooks.named (:require [clj-kondo.hooks-api :as api])) (defn named [{:keys [node]}] {:node (api/list-node [(api/token-node 'def) (api/token-node 'gen) (api/token-node 1)])})"
        root (project! (into {".clj-kondo/config.edn" "{:hooks {:analyze-call {acme/named hooks.named/named}}}"
                              ".clj-kondo/hooks/named.clj" hook
                              "src/acme.clj" "(ns acme) (defmacro named [])"}
                             (for [i (range 16)] [(str "src/app/f" i ".clj") (str "(ns app.f" i " (:require [acme])) (acme/named) (acme/named) (acme/named)")])))
        cfg (kc/project-config! (tu/temp-dir) root [])
        lookups (atom 0)
        real @#'hooks/hook-fn*]
    (with-redefs [hooks/hook-fn* (fn [ctx config ns-sym var-sym & more]
                                   (when (= 'named (symbol (name var-sym))) (swap! lookups inc))
                                   (apply real ctx config ns-sym var-sym more))]
      (reset! @#'kondo-hooks/hooks-config nil)
      (let [res (analyze/analyze-files (vec (for [i (range 16)] (str root "/src/app/f" i ".clj")))
                                       {:config cfg :mode :project :shards 8})]
        (is (every? (fn [{:keys [elements]}] (some #(= "gen" (:name %)) elements)) res) "the hook still runs")))
    (is (= 1 @lookups))))

(deftest a-retried-batch-keeps-the-analysis-started
  ;; cancelling it would stop only the outer task, leaving its clj-kondo
  ;; runs going beside the next analysis
  (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
    (let [started {:batch [{:kind :file :path "/x"}] :job {:analysis (future :analyzed)}}]
      (reset! (:in-flight ix) {:batch [{:kind :file :path "/w"}] :job {:analysis (future :other)}})
      (with-redefs [queue/next-batch (fn [& _] [{:kind :file :project-id 1 :path "/x"}])
                    indexer/start! (fn [& _] started)
                    indexer/finish! (fn [& _] :retry)]
        (is (= :retry (indexer/step! ix)))
        (is (= started @(:in-flight ix)))
        (is (not (future-cancelled? (get-in started [:job :analysis]))))))))

(deftest an-unreadable-file-is-skipped
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/ok.clj" "(ns app.ok) (defn fine [] 1)"
                        "src/app/locked.clj" "(ns app.locked) (defn hidden [] 1)"})
        locked (io/file root "src/app/locked.clj")]
    (.setReadable locked false)
    (try
      (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
        (let [done (future (sync-project! ix root))
              p (deref done 60000 :hung)]
          (is (not= :hung p) "it doesn't requeue the file forever")
          (is (contains? (visible-defs (:c ix) p) "app.ok/fine") "the rest is indexed")))
      (finally (.setReadable locked true)))))

(deftest an-error-drops-its-batch-not-the-indexer
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})]
    (with-open [ix (indexer/indexer {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)})]
      ;; thrown on the loop thread, writing the batch (an error inside
      ;; analysis arrives wrapped, as an ExecutionException)
      (with-redefs [snapshot/set-file-unit! (fn [& _] (throw (StackOverflowError.)))]
        (is (some? (sync-project! ix root)))))))

(deftest the-symbol-sweep-runs-now-and-then
  ;; it scans every symbol reference in the index: after a few edits, not
  ;; worth holding the write lock for
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [w (writer/writer c)
          p (snapshot/ensure-project! c "/a")
          file! (fn [path code] (gc-test/file! w p path code))
          sym? #(some? (db/query-value c "SELECT id FROM sym WHERE text = ?" %))]
      (file! "/a/one.clj" "(ns one) (defn only-one [] 1)")
      (snapshot/remove-file! w p "/a/one.clj")
      (gc/collect! w {})
      (is (not (sym? "only-one")) "the first collection sweeps")
      (file! "/a/two.clj" "(ns two) (defn only-two [] 1)")
      (snapshot/remove-file! w p "/a/two.clj")
      (gc/collect! w {})
      (is (sym? "only-two") "a few dead units soon after: no sweep")
      (gc/collect! w {:sweep :always})
      (is (not (sym? "only-two")) "unless asked (csl gc)"))))

(deftest an-older-daemon-slow-to-stop-doesnt-block-clients
  ;; asked to stop mid-way through a long step, it stays until the step
  ;; ends; clients go on meanwhile instead of waiting 30 s each
  (let [h (home)
        {:keys [dir daemon-lock db]} (home/paths h)
        _ (.mkdirs (io/file dir))
        held (lock/try-lock daemon-lock)]
    (try
      (with-open [c (db/open-writer db)]
        (db/execute! c "INSERT INTO daemon (id, pid, version, started_at, heartbeat_at, stop_requested)
                        VALUES (1, 4242, '0.0.1', 0, 0, 0)"))
      (let [start (System/currentTimeMillis)]
        (is (= :running (client/ensure-daemon! {:home h :version "0.2.0" :spawn! #(throw (ex-info "no" {}))})))
        (is (< (- (System/currentTimeMillis) start) 10000)))
      (finally (lock/release! held)))))
