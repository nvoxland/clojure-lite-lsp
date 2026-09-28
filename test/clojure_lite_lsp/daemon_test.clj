(ns clojure-lite-lsp.daemon-test
  (:require
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.daemon-fixture :refer [client-db fast home start! stop!]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu :refer [build-free-project! eventually]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.lang ProcessHandle]))

(def ^:private probe-ms
  "How long a lock is held as by a liveness probe, or a daemon exiting."
  300)

(deftest a-held-lock-is-held-for-this-process-too
  (let [f (str (io/file (home) "x.lock"))
        held (lock/try-lock f)]
    (is held)
    (testing "held, also for another thread of this process"
      (is (nil? (lock/try-lock f)))
      (is (lock/held? f)))
    (lock/release! held)
    (is (not (lock/held? f)))))

(deftest the-daemon-works-the-queue-and-stops-on-request
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})]
    (with-open [d (start! h {})]
      (is (eventually #(lock/held? (:daemon-lock (home/paths h)))))
      (with-open [c (client-db h)]
        (is (eventually #(= "test" (db/query-value c "SELECT version FROM daemon WHERE id = 1"))))
        (let [p (snapshot/ensure-project! c root)]
          (queue/enqueue! c p :sync "" 1)
          (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL"))))
          (testing "only one daemon at a time"
            (is (= :already-running (daemon/serve! (merge fast {:home h})))))
          (testing "a stop request ends it, and it leaves its row and lock"
            (client/request-stop! c)
            (is (= :stopped (deref d 30000 :timeout)))
            (is (nil? (db/query-value c "SELECT pid FROM daemon WHERE id = 1")))
            (is (not (lock/held? (:daemon-lock (home/paths h)))))))))))

(deftest the-daemon-exits-when-idle
  (with-open [d (start! (home) {:idle-exit-ms 200})]
    (is (= :idle (deref d 30000 :timeout)))))

(deftest the-daemon-collects-garbage-when-idle
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)" "src/b.clj" "(ns b)"})]
    (with-open [_ (start! h {:gc-after-idle-ms 100})]
      (with-open [c (client-db h)]
        (let [p (snapshot/ensure-project! c root)]
          (queue/enqueue! c p :sync "" 1)
          (is (eventually #(= 2 (db/query-value c "SELECT count(*) FROM unit"))))
          (io/delete-file (io/file root "src/a.clj"))
          (queue/enqueue! c p :delete (str root "/src/a.clj") 0)
          (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM unit")))))))))

(deftest clients-start-a-daemon-once
  (let [h (home)
        spawned (atom [])
        spawn! (fn [] (swap! spawned conj (future (daemon/serve! (merge fast {:home h})))))
        opts {:home h :version "test" :spawn! spawn!}]
    (testing "concurrent clients: one spawn"
      (is (= #{:spawned :running} (set (pmap (fn [_] (client/ensure-daemon! opts)) (range 4)))))
      (is (= 1 (count @spawned))))
    (is (= :running (client/ensure-daemon! opts)))
    (is (= 1 (count @spawned)))
    (is (stop! h @spawned))))

(deftest an-older-daemon-is-replaced
  (let [h (home)
        old (start! h {:version "0.1.0"})
        spawned (atom nil)]
    (is (eventually #(lock/held? (:daemon-lock (home/paths h)))))
    (is (= :spawned (client/ensure-daemon! {:home h :version "0.2.0"
                                            :spawn! #(reset! spawned (future (daemon/serve! (merge fast {:home h :version "0.2.0"}))))})))
    (is (= :stopped (deref old 30000 :timeout)))
    (with-open [c (client-db h)]
      (is (eventually #(= "0.2.0" (db/query-value c "SELECT version FROM daemon WHERE id = 1")))))
    (is (stop! h [@spawned]))))

(deftest a-newer-daemon-is-left-running
  ;; two editors running different clojure-lite-lsp versions: the older one must not
  ;; stop the newer daemon (and be replaced back, over and over)
  (let [h (home)]
    (with-open [newer (start! h {:version "0.2.0"})]
      (is (= :running (client/ensure-daemon! {:home h :version "0.1.0"
                                              :spawn! #(throw (ex-info "spawned" {}))})))
      (is (not (realized? newer))))))

(deftest each-schema-version-has-its-own-index
  ;; clojure-lite-lsp versions with different schemas run side by side: neither
  ;; rebuilds the other's index
  (let [h (home)
        a (home/paths h 5)
        b (home/paths h 6)]
    (doseq [k [:db :daemon-lock :spawn-lock :log]]
      (is (not= (k a) (k b)) (str k)))
    (is (= (:home a) (:home b)) "configs and extracted sources are shared")))

(deftest unused-older-indexes-are-removed
  (let [h (home)
        day-and-more (- (System/currentTimeMillis) (* 25 60 60 1000))
        make! (fn [v age] (let [{:keys [db]} (home/paths h v)]
                            (io/make-parents (io/file db))
                            (spit db "x")
                            (.setLastModified (io/file db) age)
                            (.getParentFile (io/file db))))
        unused (make! 1 day-and-more)
        recent (make! 2 (System/currentTimeMillis))
        in-use (make! 3 day-and-more)
        newer (make! 999 day-and-more)
        held (lock/try-lock (:daemon-lock (home/paths h 3)))]
    (try
      (with-open [_ (start! h {})]
        (is (not (.exists unused)))
        (is (.exists recent))
        (is (.exists in-use) "its daemon is running")
        (is (.exists newer) "a newer clojure-lite-lsp's"))
      (finally
        (lock/release! held)))))

(deftest a-liveness-probe-does-not-stop-a-starting-daemon
  ;; clients probe liveness by briefly taking daemon.lock; a daemon
  ;; starting at that moment must not conclude another daemon runs
  (let [h (home)
        probe (lock/try-lock (:daemon-lock (home/paths h)))]
    ;; the probe lets go while the daemon still tries for the lock
    (future (Thread/sleep probe-ms) (lock/release! probe))
    (let [d (future (daemon/serve! (merge fast {:home h})))]
      (is (eventually #(and (not (realized? d)) (lock/held? (:daemon-lock (home/paths h))))))
      (is (not= :already-running (deref d 500 :still-running)))
      (is (stop! h [d])))))

(deftest a-real-daemon-process
  ;; separate processes: the OS file lock and the database are all that
  ;; connect them
  (let [h (home)
        {:keys [daemon-lock log]} (home/paths h)]
    (is (= :spawned (client/ensure-daemon! {:home h})))
    (with-open [c (client-db h)]
      (try
        (is (lock/held? daemon-lock))
        (is (= :running (client/ensure-daemon! {:home h})))
        (let [pid (db/query-value c "SELECT pid FROM daemon WHERE id = 1")]
          (is (not= pid (.pid (ProcessHandle/current))) "a different process")
          (client/request-stop! c)
          (is (eventually #(not (lock/held? daemon-lock))))
          (is (eventually #(let [ph (ProcessHandle/of pid)]
                             ;; empty once the process is gone
                             (or (.isEmpty ph) (not (.isAlive ^ProcessHandle (.get ph))))))
              (slurp log)))
        (finally
          ;; a failure above mustn't leave the process running
          (client/request-stop! c))))))

(deftest work-arriving-as-the-daemon-goes-idle-is-done
  ;; a client enqueues while the idle daemon is on its way out (still
  ;; holding its lock, so the client sees it running)
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        unregister! @#'daemon/unregister!
        once (atom true)]
    ;; unregistering is the only point between "idle" and "gone"
    (with-redefs [daemon/unregister! (fn [c]
                                       (when (compare-and-set! once true false)
                                         (with-open [cc (client-db h)]
                                           (queue/enqueue! cc (snapshot/ensure-project! cc root) :sync "" 1)))
                                       (unregister! c))]
      (with-open [d (start! h {:idle-exit-ms 300})]
        (is (= :idle (deref d 30000 :timeout)))
        (with-open [c (client-db h)]
          (is (= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL"))))))))

(deftest a-daemon-gone-without-registering-is-not-waited-for
  ;; the lock held with no daemon row: a daemon exiting, or one that died
  ;; starting. Waiting for its row would stall an editor for 30 s
  (let [h (home)
        _ (.mkdirs (io/file (:dir (home/paths h))))
        held (lock/try-lock (:daemon-lock (home/paths h)))
        spawned (atom nil)
        started (System/currentTimeMillis)]
    (future (Thread/sleep probe-ms) (lock/release! held))
    (is (= :spawned (client/ensure-daemon! {:home h :version "test"
                                            :spawn! #(reset! spawned (future (daemon/serve! (merge fast {:home h}))))})))
    (is (< (- (System/currentTimeMillis) started) 10000) "not the 30 s wait for its row")
    (is (stop! h [@spawned]))))

(deftest the-daemon-survives-an-error-in-its-loop
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        step! indexer/step!
        once (atom true)]
    (with-redefs [indexer/step! (fn [ix]
                                  (if (compare-and-set! once true false)
                                    (throw (java.sql.SQLException. "database is locked"))
                                    (step! ix)))]
      (with-open [_ (start! h {:retry-ms 50})]
        (with-open [c (client-db h)]
          (queue/enqueue! c (snapshot/ensure-project! c root) :sync "" 1)
          (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL")))))))))

(deftest an-index-an-open-editor-uses-is-kept
  ;; an editor of an older version still reading its index, its daemon
  ;; long gone idle
  (let [h (home)
        {:keys [db clients-lock]} (home/paths h 3)
        _ (io/make-parents (io/file db))
        _ (spit db "x")
        _ (.setLastModified (io/file db) (- (System/currentTimeMillis) (* 48 60 60 1000)))
        editor (lock/try-lock clients-lock)]
    (try
      (with-open [_ (start! h {})]
        (is (.exists (io/file db))))
      (finally
        (lock/release! editor)))))
