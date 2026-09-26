(ns csl.daemon-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [csl.client :as client]
   [csl.daemon :as daemon]
   [csl.db :as db]
   [csl.lock :as lock]
   [csl.queue :as queue]
   [csl.snapshot :as snapshot]
   [csl.test-util :as tu]))

(defn home [] (str (tu/temp-dir)))

(defn project!
  "A project without a build tool: its .csl.edn names the source dir, so
  indexing it needs no classpath and no jars."
  [files]
  (let [root (.getCanonicalPath (tu/temp-dir))]
    (spit (io/file root ".csl.edn") "{:extra-source-paths [\"src\"]}")
    (doseq [[path code] files]
      (let [f (io/file root path)] (io/make-parents f) (spit f code)))
    root))

(def fast {:poll-ms 20 :idle-exit-ms 60000 :gc-after-idle-ms 60000 :version "test"})

(defn eventually
  "Wait (up to 30 s) for (f) to be truthy; return its value."
  [f]
  (loop [n 0]
    (or (f)
        (if (< n 1500) (do (Thread/sleep 20) (recur (inc n))) nil))))

(defn start!
  "Run a daemon in a future, returning once it is up (as clients do through
  ensure-daemon!, since the daemon creates the schema)."
  [h opts]
  (let [d (future (daemon/run! (merge fast {:home h} opts)))]
    (eventually #(or (realized? d)
                     (with-open [c (db/open-client (:db (daemon/paths h)))]
                       (try (db/query-value c "SELECT version FROM daemon WHERE id = 1")
                            (catch java.sql.SQLException _ nil)))))
    d))

(defn client-db [h] (db/open-client (:db (daemon/paths h))))

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
        root (project! {"src/a.clj" "(ns a) (defn f [] 1)"})]
    (is (eventually #(lock/held? (:daemon-lock (daemon/paths h)))))
    (with-open [c (client-db h)]
      (is (eventually #(= "test" (db/query-value c "SELECT version FROM daemon WHERE id = 1"))))
      (let [p (snapshot/ensure-project! c root)]
        (queue/enqueue! c p :sync "" 1)
        (is (eventually #(= 1 (db/query-value c "SELECT count(*) FROM project_file WHERE unit_id IS NOT NULL"))))
        (testing "only one daemon at a time"
          (is (= :already-running (daemon/run! (merge fast {:home h})))))
        (client/request-stop! c)
        (is (= :stopped (deref d 30000 :timeout)))
        (is (nil? (db/query-value c "SELECT pid FROM daemon WHERE id = 1")))
        (is (not (lock/held? (:daemon-lock (daemon/paths h)))))))))

(deftest the-daemon-exits-when-idle
  (let [d (start! (home) {:idle-exit-ms 200})]
    (is (= :idle (deref d 30000 :timeout)))))

(deftest the-daemon-collects-garbage-when-idle
  (let [h (home)
        d (start! h {:gc-after-idle-ms 100})
        root (project! {"src/a.clj" "(ns a) (defn f [] 1)" "src/b.clj" "(ns b)"})]
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
        spawn! (fn [] (swap! spawned conj (future (daemon/run! (merge fast {:home h})))))
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
        old (start! h {:version "old"})
        spawned (atom nil)]
    (is (eventually #(lock/held? (:daemon-lock (daemon/paths h)))))
    (is (= :spawned (client/ensure-daemon! {:home h :version "new"
                                            :spawn! #(reset! spawned (future (daemon/run! (merge fast {:home h :version "new"}))))})))
    (is (= :stopped (deref old 30000 :timeout)))
    (with-open [c (client-db h)]
      (is (eventually #(= "new" (db/query-value c "SELECT version FROM daemon WHERE id = 1"))))
      (client/request-stop! c))
    (deref @spawned 30000 :timeout)))

(deftest a-liveness-probe-does-not-stop-a-starting-daemon
  ;; clients probe liveness by briefly taking daemon.lock; a daemon
  ;; starting at that moment must not conclude another daemon runs
  (let [h (home)
        probe (lock/try-lock (:daemon-lock (daemon/paths h)))]
    (future (Thread/sleep 200) (lock/release! probe))
    (let [d (future (daemon/run! (merge fast {:home h})))]
      (is (eventually #(and (not (realized? d)) (lock/held? (:daemon-lock (daemon/paths h))))))
      (is (not= :already-running (deref d 500 :still-running)))
      (with-open [c (client-db h)] (client/request-stop! c))
      (is (= :stopped (deref d 30000 :timeout))))))

(deftest a-real-daemon-process
  ;; separate processes: the OS file lock and the database are all that
  ;; connect them
  (let [h (home)
        {:keys [daemon-lock log]} (daemon/paths h)]
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
