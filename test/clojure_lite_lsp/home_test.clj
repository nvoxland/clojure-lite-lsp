(ns clojure-lite-lsp.home-test
  (:require
   [clojure-lite-lsp.home :as home]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(defn- sys [os env]
  {:os os :user-home "/u" :env env})

(defn- path [& parts] (str (apply io/file parts)))

(deftest each-os-keeps-caches-and-data-in-its-own-place
  (testing "macOS"
    (is (= (path "/u" "Library" "Caches" "clojure-lite-lsp") (home/dir (sys "Mac OS X" {}))))
    (is (= (path "/u" "Library" "Application Support" "clojure-lite-lsp") (home/data-dir (sys "Mac OS X" {})))))
  (testing "Linux: the XDG base directories"
    (is (= (path "/u" ".cache" "clojure-lite-lsp") (home/dir (sys "Linux" {}))))
    (is (= (path "/u" ".local" "share" "clojure-lite-lsp") (home/data-dir (sys "Linux" {}))))
    (is (= (path "/xc" "clojure-lite-lsp") (home/dir (sys "Linux" {"XDG_CACHE_HOME" "/xc"}))))
    (is (= (path "/xd" "clojure-lite-lsp") (home/data-dir (sys "Linux" {"XDG_DATA_HOME" "/xd"}))))
    (testing "a relative XDG path is to be ignored"
      (is (= (path "/u" ".cache" "clojure-lite-lsp") (home/dir (sys "Linux" {"XDG_CACHE_HOME" "rel"}))))))
  (testing "Windows"
    (is (= (path "C:\\L" "clojure-lite-lsp" "Cache") (home/dir (sys "Windows 11" {"LOCALAPPDATA" "C:\\L"}))))
    (is (= (path "C:\\L" "clojure-lite-lsp" "Data") (home/data-dir (sys "Windows 11" {"LOCALAPPDATA" "C:\\L"}))))))

(deftest clojure-lite-lsp-home-puts-everything-in-one-place
  (let [env {"CLOJURE_LITE_LSP_HOME" "/try" "XDG_CACHE_HOME" "/xc"}]
    (is (= "/try" (home/dir (sys "Linux" env))))
    (is (= "/try" (home/data-dir (sys "Mac OS X" env))))))
