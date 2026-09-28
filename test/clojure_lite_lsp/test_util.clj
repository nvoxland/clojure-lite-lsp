(ns clojure-lite-lsp.test-util
  "What tests make: temporary directories, projects and jars; and waiting
  for something to happen."
  (:require
   [clojure-lite-lsp.lock :as lock]
   [clojure.java.io :as io])
  (:import
   [java.io File]
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]
   [java.util.jar JarEntry JarOutputStream]))

(defonce ^:private temp-dirs (atom []))

(defn- delete-tree! [^File f]
  (run! #(.delete ^File %) (reverse (file-seq f))))

(defonce ^:private cleanup
  ;; installed on first use, once
  (delay (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable #(run! delete-tree! @temp-dirs)))))

(defn temp-dir
  "A fresh temporary directory, deleted with its contents when the JVM
  exits."
  ^File []
  (let [d (.toFile (Files/createTempDirectory "clojure-lite-lsp-test" (make-array FileAttribute 0)))]
    (force cleanup)
    (swap! temp-dirs conj d)
    d))

(defn temp-db-path
  "Where a fresh index can be created."
  []
  (str (io/file (temp-dir) "index.db")))

(defn eventually
  "Wait for (f) to be truthy, up to `timeout-ms` (default 30 s): its value,
  or nil."
  ([f] (eventually f 30000))
  ([f timeout-ms] (lock/poll f timeout-ms 20)))

(defn project!
  "A temp project dir containing `files` ({relative-path content}, content
  :dir for an empty directory)."
  [files]
  (let [root (.getCanonicalFile (temp-dir))]
    (doseq [[path content] files]
      (let [f (io/file root path)]
        (io/make-parents f)
        (if (= :dir content) (.mkdirs f) (spit f content))))
    (str root)))

(def src-deps
  "A deps.edn whose sources are src/."
  {"deps.edn" "{:paths [\"src\"]}"})

(defn build-free-project!
  "A project without a build tool: its .clojure-lite-lsp.edn names the
  source dir, so indexing it needs no classpath and no jars."
  [files]
  (project! (assoc files ".clojure-lite-lsp.edn" "{:extra-source-paths [\"src\"]}")))

(defn jar!
  "A jar file with `entries` ({path content})."
  [entries]
  (let [f (io/file (temp-dir) "lib.jar")]
    (with-open [out (JarOutputStream. (io/output-stream f))]
      (doseq [[path content] entries]
        (.putNextEntry out (JarEntry. ^String path))
        (.write out (.getBytes ^String content "UTF-8"))
        (.closeEntry out)))
    (str f)))

(defn maven-jar!
  "A jar for group/artifact that depends on `deps` ([group artifact]) and
  has `extra` entries."
  [group artifact deps extra]
  (jar! (merge {(str "META-INF/maven/" group "/" artifact "/pom.properties")
                (str "groupId=" group "\nartifactId=" artifact "\nversion=1.0\n")
                (str "META-INF/maven/" group "/" artifact "/pom.xml")
                ;; a valid Maven model: tools.deps reads poms inside local jars
                (str "<project><modelVersion>4.0.0</modelVersion>"
                     "<groupId>" group "</groupId><artifactId>" artifact "</artifactId><version>1.0</version>"
                     "<dependencies>"
                     (apply str (for [[g a] deps]
                                  (str "<dependency><groupId>" g "</groupId><artifactId>" a "</artifactId>"
                                       "<version>1.0</version></dependency>")))
                     "</dependencies></project>")}
               extra)))

(defn files-in
  "The paths of the files under `dir`, relative to it."
  [dir]
  (let [base (count (str dir "/"))]
    (into #{} (comp (filter #(.isFile ^File %)) (map #(subs (str %) base))) (file-seq (io/file dir)))))
