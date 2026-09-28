(ns clojure-lite-lsp.sources-test
  (:require
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.kondo-config-test :refer [jar!]]
   [clojure-lite-lsp.sources :as sources]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest extracts-jar-entries-as-read-only-files
  (let [home (str (tu/temp-dir))
        j (jar! {"acme/core.clj" "(ns acme.core)\n(defn f [] 1)\n"})
        h (digest/sha256 (io/file j))
        hex (apply str (map #(format "%02x" %) h))
        loc {:path j :entry "acme/core.clj" :jar-hash h}
        path (sources/extract! home loc)]
    ;; canonical, as editors send them back (the temp dir is under a symlink)
    (is (= (.getCanonicalPath (io/file home "sources" hex "acme" "core.clj")) path))
    (is (= "(ns acme.core)\n(defn f [] 1)\n" (slurp path)))
    (is (not (.canWrite (io/file path))) "read-only: it's the library's source, not the user's")
    (testing "again: the same file, not rewritten"
      (let [mtime (.lastModified (io/file path))]
        (Thread/sleep 10)
        (is (= path (sources/extract! home loc)))
        (is (= mtime (.lastModified (io/file path))))))
    (testing "an extracted file says where it came from"
      (is (= {:jar-hash-hex hex :entry "acme/core.clj"} (sources/source-of home path)))
      (is (nil? (sources/source-of home "/somewhere/else.clj")))
      (is (= [1 2 -1] (vec (digest/unhex "0102ff")))))))

(deftest extraction-refuses-what-it-cant-vouch-for
  (let [home (str (tu/temp-dir))
        j (jar! {"acme/core.clj" "(ns acme.core)" "../../../evil.clj" "(ns evil)"})
        h (digest/sha256 (io/file j))]
    (testing "an entry naming a path outside the sources dir (zip-slip)"
      (is (nil? (sources/extract! home {:path j :entry "../../../evil.clj" :jar-hash h})))
      (is (not (.exists (io/file home "evil.clj")))))
    (testing "an entry the jar doesn't have"
      (is (nil? (sources/extract! home {:path j :entry "acme/missing.clj" :jar-hash h}))))
    (testing "a jar that's gone"
      (is (nil? (sources/extract! home {:path "/nowhere/x.jar" :entry "acme/core.clj" :jar-hash h}))))
    (testing "a jar replaced since it was indexed: its content isn't what the hash says"
      (is (nil? (sources/extract! home {:path j :entry "acme/core.clj" :jar-hash (byte-array [1 2 3])}))))))

(deftest config-exports-stay-in-their-dir
  (let [cache (str (tu/temp-dir))]
    (is (thrown? Exception (kc/materialize! cache {"../../escaped.edn" (.getBytes "{}")})))
    (is (not (.exists (io/file cache "escaped.edn"))))))
