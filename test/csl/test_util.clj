(ns csl.test-util
  (:require
   [clojure.java.io :as io])
  (:import
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

(defn temp-dir
  "A fresh temporary directory, deleted when the JVM exits."
  ^java.io.File []
  (let [d (.toFile (Files/createTempDirectory "csl-test" (make-array FileAttribute 0)))]
    (.deleteOnExit d)
    d))

(defn temp-db-path []
  (str (io/file (temp-dir) "index.db")))
