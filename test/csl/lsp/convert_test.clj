(ns csl.lsp.convert-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.lsp.convert :as convert]))

(deftest file-uris
  (is (= "file:///a/b%20c/d.clj" (convert/path->uri "/a/b c/d.clj")))
  (is (= "/a/b c/d.clj" (convert/uri->path "file:///a/b%20c/d.clj")))
  (is (= "/a/b.clj" (convert/uri->path "file:/a/b.clj")) "the short form some clients send"))

(deftest jar-entries
  (testing "jar: URIs by default"
    (is (= "jar:file:///m2/x.jar!/acme/core.clj"
           (convert/location-uri {:path "/m2/x.jar" :entry "acme/core.clj"} {})))
    (is (= {:path "/m2/x.jar" :entry "acme/core.clj"} (convert/uri->location "jar:file:///m2/x.jar!/acme/core.clj"))))
  (testing "zipfile: URIs for clients that open those"
    (is (= "zipfile:///m2/x.jar::acme/core.clj"
           (convert/location-uri {:path "/m2/x.jar" :entry "acme/core.clj"} {:dependency-scheme "zipfile"})))
    (is (= {:path "/m2/x.jar" :entry "acme/core.clj"} (convert/uri->location "zipfile:///m2/x.jar::acme/core.clj")))))

(deftest positions
  ;; clj-kondo: 1-based rows and columns, end exclusive, columns in UTF-16
  ;; units (as LSP's default encoding)
  (is (= {:start {:line 0 :character 4} :end {:line 0 :character 7}} (convert/range [1 5 1 8])))
  (is (= [3 11] (convert/->kondo {:line 2 :character 10}))))

(deftest locations
  (is (= {:uri "file:///p/src/a.clj" :range {:start {:line 1 :character 6} :end {:line 1 :character 7}}}
         (convert/location {:path "/p/src/a.clj" :pos [2 7 2 8]} {}))))
