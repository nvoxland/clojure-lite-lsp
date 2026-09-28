(ns clojure-lite-lsp.fingerprint-test
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.fingerprint :as fingerprint]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.security MessageDigest]))

(defn sha256 [^String s]
  (vec (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))))

(deftest content-hash-is-memoized-by-mtime-and-size
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (let [f (io/file (tu/temp-dir) "a.clj")
          _ (spit f "(ns a)")
          mtime (.lastModified f)]
      (testing "the SHA-256 of the file's bytes"
        (is (= (sha256 "(ns a)") (vec (fingerprint/content-hash! c (str f))))))
      (testing "an unchanged mtime and size is trusted, without reading the file"
        (spit f "(ns b)")
        (.setLastModified f mtime)
        (is (= (sha256 "(ns a)") (vec (fingerprint/content-hash! c (str f))))))
      (testing "a changed mtime rehashes"
        (.setLastModified f (+ mtime 5000))
        (is (= (sha256 "(ns b)") (vec (fingerprint/content-hash! c (str f))))))
      (testing "a changed size rehashes"
        (spit f "(ns bigger)")
        (.setLastModified f (+ mtime 5000))
        (is (= (sha256 "(ns bigger)") (vec (fingerprint/content-hash! c (str f)))))))))

(deftest missing-files-have-no-hash
  (with-open [c (db/open-writer (tu/temp-db-path))]
    (is (nil? (fingerprint/content-hash! c "/no/such/file.clj")))))
