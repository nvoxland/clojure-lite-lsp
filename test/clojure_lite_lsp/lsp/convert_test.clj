(ns clojure-lite-lsp.lsp.convert-test
  (:require
   [clojure-lite-lsp.lsp.convert :as convert]
   [clojure.test :refer [deftest is testing]]))

(deftest paths-and-file-uris-convert-both-ways
  (is (= "file:///a/b%20c/d.clj" (convert/path->uri "/a/b c/d.clj")))
  (is (= "/a/b c/d.clj" (convert/uri->path "file:///a/b%20c/d.clj")))
  (is (= "/a/b.clj" (convert/uri->path "file:/a/b.clj")) "the short form some clients send"))

(deftest jar-entries-get-jar-or-zipfile-uris
  (testing "jar: URIs by default"
    (is (= "jar:file:///m2/x.jar!/acme/core.clj"
           (convert/location-uri {:path "/m2/x.jar" :entry "acme/core.clj"} {}))))
  (testing "zipfile: URIs for clients that open those"
    (is (= "zipfile:///m2/x.jar::acme/core.clj"
           (convert/location-uri {:path "/m2/x.jar" :entry "acme/core.clj"} {:dependency-scheme "zipfile"})))))

(deftest positions-convert-between-kondo-and-lsp
  ;; clj-kondo: 1-based rows and columns, end exclusive, columns in UTF-16
  ;; units (as LSP's default encoding)
  (is (= {:start {:line 0 :character 4} :end {:line 0 :character 7}} (convert/range [1 5 1 8])))
  (is (= [3 11] (convert/->kondo {:line 2 :character 10}))))

(deftest locations-become-lsp-locations
  (is (= {:uri "file:///p/src/a.clj" :range {:start {:line 1 :character 6} :end {:line 1 :character 7}}}
         (convert/location {:path "/p/src/a.clj" :pos [2 7 2 8]} {})))
  (testing "without a position (a Java class's file): its start"
    (is (= {:start {:line 0 :character 0} :end {:line 0 :character 0}}
           (:range (convert/location {:path "/p/A.java"} {}))))))
