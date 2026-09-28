(ns clojure-lite-lsp.daemon-test
  (:require
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.daemon-fixture :refer [home fast client-db start!]]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu :refer [build-free-project! eventually]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest locks
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
        d (start! h {})
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})]
    (is (eventually #(lock/held? (:daemon-lock (home/paths h)))))
    (with-open [c (client-db h)]
      (is (eventually #(= "test" (db/query-value c "SELECT version FROM daemon WHERE id = 1"))))
      (let [p (snapshot/ensure-project! c root)]
        (queue/enqueue! c p :sync "" 1)
        (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL"))))
        (testing "only one daemon at a time"
          (is (= :already-running (daemon/serve! (merge fast {:home h})))))
        (client/request-stop! c)
        (is (= :stopped (deref d 30000 :timeout)))
        (is (nil? (db/query-value c "SELECT pid FROM daemon WHERE id = 1")))
        (is (not (lock/held? (:daemon-lock (home/paths h)))))))))

(deftest the-daemon-exits-when-idle
  (let [d (start! (home) {:idle-exit-ms 200})]
    (is (= :idle (deref d 30000 :timeout)))))

(deftest the-daemon-collects-garbage-when-idle
  (let [h (home)
        d (start! h {:gc-after-idle-ms 100})
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)" "src/b.clj" "(ns b)"})]
    (with-open [c (client-db h)]
      (let [p (snapshot/ensure-project! c root)]
        (queue/enqueue! c p :sync "" 1)
        (is (eventually #(= 2 (db/query-value c "SELECT count(*) FROM unit"))))
        (io/delete-file (io/file root "src/a.clj"))
        (queue/enqueue! c p :delete (str root "/src/a.clj") 0)
        (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM unit"))))
        (client/request-stop! c)
        (deref d 30000 :timeout)))))

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
    (with-open [c (client-db h)] (client/request-stop! c))
    (deref (first @spawned) 30000 :timeout)))

(deftest an-older-daemon-is-replaced
  (let [h (home)
        old (start! h {:version "0.1.0"})
        spawned (atom nil)]
    (is (eventually #(lock/held? (:daemon-lock (home/paths h)))))
    (is (= :spawned (client/ensure-daemon! {:home h :version "0.2.0"
                                            :spawn! #(reset! spawned (future (daemon/serve! (merge fast {:home h :version "0.2.0"}))))})))
    (is (= :stopped (deref old 30000 :timeout)))
    (with-open [c (client-db h)]
      (is (eventually #(= "0.2.0" (db/query-value c "SELECT version FROM daemon WHERE id = 1"))))
      (client/request-stop! c))
    (deref @spawned 30000 :timeout)))

(deftest a-newer-daemon-is-left-running
  ;; two editors running different clojure-lite-lsp versions: the older one must not
  ;; stop the newer daemon (and be replaced back, over and over)
  (let [h (home)
        newer (start! h {:version "0.2.0"})]
    (is (= :running (client/ensure-daemon! {:home h :version "0.1.0"
                                            :spawn! #(throw (ex-info "spawned" {}))})))
    (is (not (realized? newer)))
    (with-open [c (client-db h)] (client/request-stop! c))
    (deref newer 30000 :timeout)))

(deftest versions-compare-by-number
  (is (client/older? "0.1.0" "0.2.0"))
  (is (client/older? "0.9.0" "0.10.0"))
  (is (client/older? "0.2.0-SNAPSHOT" "0.2.0"))
  (is (not (client/older? "0.2.0" "0.2.0")))
  (is (not (client/older? "0.2.0" "0.1.9"))))

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
        held (lock/try-lock (:daemon-lock (home/paths h 3)))
        d (start! h {})]
    (try
      (is (not (.exists unused)))
      (is (.exists recent))
      (is (.exists in-use) "its daemon is running")
      (is (.exists newer) "a newer clojure-lite-lsp's")
      (finally
        (lock/release! held)
        (with-open [c (client-db h)] (client/request-stop! c))
        (deref d 30000 :timeout)))))

(deftest a-liveness-probe-does-not-stop-a-starting-daemon
  ;; clients probe liveness by briefly taking daemon.lock; a daemon
  ;; starting at that moment must not conclude another daemon runs
  (let [h (home)
        probe (lock/try-lock (:daemon-lock (home/paths h)))]
    (future (Thread/sleep 200) (lock/release! probe))
    (let [d (future (daemon/serve! (merge fast {:home h})))]
      (is (eventually #(and (not (realized? d)) (lock/held? (:daemon-lock (home/paths h))))))
      (is (not= :already-running (deref d 500 :still-running)))
      (with-open [c (client-db h)] (client/request-stop! c))
      (is (= :stopped (deref d 30000 :timeout))))))

(deftest a-real-daemon-process
  ;; separate processes: the OS file lock and the database are all that
  ;; connect them
  (let [h (home)
        {:keys [daemon-lock log]} (home/paths h)]
    (is (= :spawned (client/ensure-daemon! {:home h})))
    (is (lock/held? daemon-lock))
    (is (= :running (client/ensure-daemon! {:home h})))
    (with-open [c (client-db h)]
      (let [pid (db/query-value c "SELECT pid FROM daemon WHERE id = 1")]
        (is (not= pid (.pid (java.lang.ProcessHandle/current))) "a different process")
        (client/request-stop! c)
        (is (eventually #(not (lock/held? daemon-lock))))
        (is (eventually #(let [ph (java.lang.ProcessHandle/of pid)]
                                  ;; empty once the process is gone
                           (or (.isEmpty ph) (not (.isAlive ^java.lang.ProcessHandle (.get ph))))))
            (slurp log))))))

(deftest the-daemon-log-is-rotated
  (let [log (io/file (home) "daemon.log")]
    (spit log (apply str (repeat (* 6 1024 1024) "x")))
    (client/rotate-log! (str log))
    (is (not (.exists log)))
    (is (= (* 6 1024 1024) (.length (io/file (str log ".1")))))
    (testing "a small log is left alone"
      (spit log "small")
      (client/rotate-log! (str log))
      (is (= "small" (slurp log))))))

(deftest work-arriving-as-the-daemon-goes-idle-is-done
  ;; a client enqueues while the idle daemon is on its way out (still
  ;; holding its lock, so the client sees it running)
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        real @#'daemon/unregister!
        once (atom true)]
    (with-redefs [daemon/unregister! (fn [c]
                                       (when (compare-and-set! once true false)
                                         (with-open [cc (client-db h)]
                                           (queue/enqueue! cc (snapshot/ensure-project! cc root) :sync "" 1)))
                                       (real c))]
      (let [d (start! h {:idle-exit-ms 300})]
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
    (future (Thread/sleep 300) (lock/release! held))
    (is (= :spawned (client/ensure-daemon! {:home h :version "test"
                                            :spawn! #(reset! spawned (future (daemon/serve! (merge fast {:home h}))))})))
    (is (< (- (System/currentTimeMillis) started) 10000))
    (with-open [c (client-db h)] (client/request-stop! c))
    (deref @spawned 30000 :timeout)))

(deftest the-daemon-survives-an-error-in-its-loop
  (let [h (home)
        root (build-free-project! {"src/a.clj" "(ns a) (defn f [] 1)"})
        real indexer/step!
        once (atom true)]
    (with-redefs [indexer/step! (fn [ix]
                                  (if (compare-and-set! once true false)
                                    (throw (java.sql.SQLException. "database is locked"))
                                    (real ix)))]
      (let [d (start! h {:retry-ms 50})]
        (with-open [c (client-db h)]
          (queue/enqueue! c (snapshot/ensure-project! c root) :sync "" 1)
          (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL"))))
          (client/request-stop! c))
        (deref d 30000 :timeout)))))

(deftest an-index-an-open-editor-uses-is-kept
  ;; an editor of an older version still reading its index, its daemon
  ;; long gone idle
  (let [h (home)
        {:keys [db clients-lock]} (home/paths h 3)
        _ (io/make-parents (io/file db))
        _ (spit db "x")
        _ (.setLastModified (io/file db) (- (System/currentTimeMillis) (* 48 60 60 1000)))
        editor (lock/try-lock clients-lock)
        d (start! h {})]
    (try
      (is (.exists (io/file db)))
      (finally
        (lock/release! editor)
        (with-open [c (client-db h)] (client/request-stop! c))
        (deref d 30000 :timeout)))))
