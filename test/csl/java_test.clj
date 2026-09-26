(ns csl.java-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [csl.classpath-test :refer [project!]]
   [csl.java :as java]
   [csl.kondo-config-test :refer [jar!]]
   [csl.test-util :as tu]))

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
      (is (.startsWith ^String path (str home "/sources/")) "extracted like library files")
      (is (= widget (slurp path)))
      (is (= [4 14] (vec (take 2 pos)))))))

(deftest jdk-sources
  (let [src-zip (jar! {"java.base/java/io/File.java" "package java.io;\n\npublic class File {}\n"})
        {:keys [path pos]} (java/source-location {:home (str (tu/temp-dir)) :jdk-src src-zip} "java.io.File")]
    (is (.endsWith ^String path "/java.base/java/io/File.java"))
    (is (= [3 14] (vec (take 2 pos))))))

(deftest nothing-when-there-are-no-sources
  (is (nil? (java/source-location {:home (str (tu/temp-dir))} "acme.Nowhere"))))
