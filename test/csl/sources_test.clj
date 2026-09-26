(ns csl.sources-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [csl.kondo-config-test :refer [jar!]]
   [csl.sources :as sources]
   [csl.test-util :as tu]))

(deftest extracts-jar-entries-as-read-only-files
  (let [home (str (tu/temp-dir))
        j (jar! {"acme/core.clj" "(ns acme.core)\n(defn f [] 1)\n"})
        loc {:path j :entry "acme/core.clj" :jar-hash (byte-array [1 2 255])}
        path (sources/extract! home loc)]
    ;; canonical, as editors send them back (the temp dir is under a symlink)
    (is (= (.getCanonicalPath (io/file home "sources" "0102ff" "acme" "core.clj")) path))
    (is (= "(ns acme.core)\n(defn f [] 1)\n" (slurp path)))
    (is (not (.canWrite (io/file path))) "read-only: it's the library's source, not the user's")
    (testing "again: the same file, not rewritten"
      (let [mtime (.lastModified (io/file path))]
        (Thread/sleep 10)
        (is (= path (sources/extract! home loc)))
        (is (= mtime (.lastModified (io/file path))))))
    (testing "an extracted file says where it came from"
      (is (= {:jar-hash-hex "0102ff" :entry "acme/core.clj"} (sources/source-of home path)))
      (is (nil? (sources/source-of home "/somewhere/else.clj")))
      (is (= [1 2 -1] (vec (sources/unhex "0102ff")))))))
