(ns csl.queue-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.db :as db]
   [csl.queue :as queue]
   [csl.snapshot :as snapshot]
   [csl.test-util :as tu]))

(defn with-db [f]
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (f c (snapshot/ensure-project! c "/p1") (snapshot/ensure-project! c "/p2"))))

(defn paths [batch] (set (map :path batch)))

(deftest enqueue-only-raises-priority
  (with-db
    (fn [c p _]
      (queue/enqueue! c p :file "/p1/a.clj" 1)
      (queue/enqueue! c p :file "/p1/a.clj" 0)
      (queue/enqueue! c p :file "/p1/a.clj" 1)
      (is (= [0] (map :priority (queue/next-batch c {})))))))

(deftest batches-take-the-top-priority-of-one-project-and-kind
  (with-db
    (fn [c p1 p2]
      (queue/enqueue! c p1 :file "/p1/b.clj" 1)
      (queue/enqueue! c p1 :jar "/m2/x.jar" 2)
      (queue/enqueue! c p2 :file "/p2/c.clj" 0)
      (queue/enqueue! c p1 :file "/p1/a.clj" 1)
      (let [b1 (queue/next-batch c {})]
        (is (= #{"/p2/c.clj"} (paths b1)))
        (queue/done! c b1))
      (let [b2 (queue/next-batch c {})]
        (is (= #{"/p1/a.clj" "/p1/b.clj"} (paths b2)))
        (is (every? #(= [p1 :file] [(:project-id %) (:kind %)]) b2))
        (queue/done! c b2))
      (is (= #{"/m2/x.jar"} (paths (queue/next-batch c {})))))))

(deftest batch-size-limits
  (with-db
    (fn [c p _]
      (doseq [i (range 5)] (queue/enqueue! c p :file (str "/p1/" i ".clj") 1))
      (is (= 2 (count (queue/next-batch c {:file 2})))))))

(deftest a-request-re-enqueued-while-being-processed-is-kept
  (with-db
    (fn [c p _]
      (queue/enqueue! c p :file "/p1/a.clj" 1)
      (let [batch (queue/next-batch c {})]
        ;; saved again while the batch is being analyzed (no delay: clients
        ;; and the daemon are different processes, so no clock to rely on)
        (queue/enqueue! c p :file "/p1/a.clj" 1)
        (queue/done! c batch)
        (is (= #{"/p1/a.clj"} (paths (queue/next-batch c {}))))))))

(deftest order-is-first-in-first-out
  ;; the order comes from the database, not any one process's clock
  (with-db
    (fn [c p _]
      (queue/enqueue! c p :file "/p1/first.clj" 1)
      (queue/enqueue! c p :file "/p1/second.clj" 1)
      (is (= ["/p1/first.clj" "/p1/second.clj"] (map :path (queue/next-batch c {})))))))

(deftest counts-for-progress
  (with-db
    (fn [c p _]
      (queue/enqueue! c p :file "/p1/a.clj" 1)
      (queue/enqueue! c p :jar "/m2/x.jar" 2)
      (is (= 2 (queue/pending-count c p))))))

(deftest in-flight-requests-are-skipped
  ;; the indexer takes the next batch before the previous one is done
  (with-db
    (fn [c p _]
      (doseq [f ["/p1/a.clj" "/p1/b.clj" "/p1/c.clj"]] (queue/enqueue! c p :file f 1))
      (let [first-batch (queue/next-batch c {:file 1})]
        (is (= ["/p1/a.clj"] (map :path first-batch)))
        (is (= ["/p1/b.clj"] (map :path (queue/next-batch c {:file 1} (map queue/request-key first-batch)))))))))

(deftest enqueueing-keeps-a-project-alive
  ;; GC drops projects not seen for a while: an editor that keeps working
  ;; on one must count as seeing it
  (with-db (fn [c p1 _]
             (db/execute! c "UPDATE project SET last_seen = 0 WHERE id = ?" p1)
             (queue/enqueue! c p1 :file "/p1/a.clj" 1)
             (is (pos? (db/query-value c "SELECT last_seen FROM project WHERE id = ?" p1))))))
