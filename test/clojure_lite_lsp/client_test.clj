(ns clojure-lite-lsp.client-test
  (:require
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon-fixture :refer [home]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest versions-compare-by-number
  (is (client/older? "0.1.0" "0.2.0"))
  (is (client/older? "0.9.0" "0.10.0"))
  (is (client/older? "0.2.0-SNAPSHOT" "0.2.0"))
  (is (not (client/older? "0.2.0" "0.2.0")))
  (is (not (client/older? "0.2.0" "0.1.9"))))

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
