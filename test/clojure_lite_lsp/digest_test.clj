(ns clojure-lite-lsp.digest-test
  (:require
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]))

(def abc "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

(deftest hashes-its-parts-in-order
  (is (= abc (digest/hex (digest/sha256 "abc"))))
  (is (= abc (digest/hex (digest/sha256 "a" (.getBytes "b") (io/input-stream (.getBytes "c"))))))
  (let [f (io/file (tu/temp-dir) "f")]
    (spit f "abc")
    (is (= abc (digest/hex (digest/sha256 f))))))

(deftest hex-round-trips
  (is (= [0 10 -1] (vec (digest/unhex (digest/hex (byte-array [0 10 -1])))))))
