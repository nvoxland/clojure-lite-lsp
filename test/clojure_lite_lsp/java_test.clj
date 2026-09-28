(ns clojure-lite-lsp.java-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.java :as java]
   [clojure-lite-lsp.kondo-config-test :refer [jar!]]
   [clojure-lite-lsp.test-util :as tu]))

(def widget "package acme;\n\n/** A widget. */\npublic class Widget {\n  public static class Part {}\n}\n")

(deftest project-java-sources
  (let [root (project! {"java/acme/Widget.java" widget})]
    (is (= [(str root "/java/acme/Widget.java") [4 14]]
           ((juxt :path (comp vec (partial take 2) :pos))
            (java/source-location {:home (str (tu/temp-dir)) :source-dirs [(str root "/java")]} "acme.Widget"))))
    (testing "a nested class is in its outer class's file"
      (is (= (str root "/java/acme/Widget.java")
             (:path (java/source-location {:home (str (tu/temp-dir)) :source-dirs [(str root "/java")]} "acme.Widget$Part")))))))

(deftest sources-jars-next-to-class-jars
  (let [dir (tu/temp-dir)
        classes (io/file dir "widget-1.0.jar")
        sources (io/file dir "widget-1.0-sources.jar")
        home (str (tu/temp-dir))]
    (io/copy (io/file (jar! {"acme/Widget.class" "binary"})) classes)
    (io/copy (io/file (jar! {"acme/Widget.java" widget})) sources)
    (let [{:keys [path pos]} (java/source-location {:home home :class-jars [(str classes)]} "acme.Widget")]
      (is (.startsWith ^String path (str (.getCanonicalPath (io/file home)) "/sources/")) "extracted like library files")
      (is (= widget (slurp path)))
      (is (= [4 14] (vec (take 2 pos)))))))

(deftest jdk-sources
  (let [src-zip (jar! {"java.base/java/io/File.java" "package java.io;\n\npublic class File {}\n"})
        {:keys [path pos]} (java/source-location {:home (str (tu/temp-dir)) :jdk-src src-zip} "java.io.File")]
    (is (.endsWith ^String path "/java.base/java/io/File.java"))
    (is (= [3 14] (vec (take 2 pos))))))

(deftest nothing-when-there-are-no-sources
  (is (nil? (java/source-location {:home (str (tu/temp-dir))} "acme.Nowhere"))))

(deftest the-jdk-sources-are-looked-for-where-jdks-keep-them
  (let [home (fn [layout] (let [h (str (clojure-lite-lsp.test-util/temp-dir))]
                            (io/make-parents (io/file h layout)) (spit (io/file h layout) "") h))
        modern (home "lib/src.zip")
        jdk8 (home "src.zip")]
    (is (= (str modern "/lib/src.zip") (java/jdk-src-in {:java-home-env modern})))
    (is (= (str jdk8 "/src.zip") (java/jdk-src-in {:java-home-env jdk8})) "JDK 8's layout")
    (is (= (str modern "/lib/src.zip") (java/jdk-src-in {:run (fn [cmd] (when (= ["/usr/libexec/java_home"] cmd) modern))}))
        "macOS: /usr/libexec/java_home when JAVA_HOME isn't set")
    (is (nil? (java/jdk-src-in {:run (constantly nil)})))))
