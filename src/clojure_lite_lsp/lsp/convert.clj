(ns clojure-lite-lsp.lsp.convert
  "Between clojure-lite-lsp's positions and paths and LSP's.

  clj-kondo's positions are 1-based rows and columns, end exclusive, with
  columns in UTF-16 code units (checked: an emoji counts as two), which is
  LSP's default position encoding. So converting is subtracting one.

  Files inside jars get `jar:file:///x.jar!/entry` URIs, or for clients
  that open those, `zipfile:///x.jar::entry`."
  (:refer-clojure :exclude [range])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.net URI]))

(set! *warn-on-reflection* true)

(defn path->uri
  "A path as a file:/// URI (File.toURI gives the short file:/ form, which
  some clients don't accept)."
  [path]
  (str (.toUri (.toPath (File. ^String path)))))

(defn uri->path
  "The path of a file: URI; nil for other URIs (e.g. jar: entries)."
  ^String [uri]
  (when (and uri (str/starts-with? uri "file:"))
    (.getPath (URI. (str/replace uri #"^file:(?!//)" "file://")))))

(defn location-uri
  "The URI of a location's file: a file, or {:path jar :entry} inside a jar."
  [{:keys [path entry]} {:keys [dependency-scheme]}]
  (cond
    (nil? entry) (path->uri path)
    (= "zipfile" dependency-scheme) (str "zipfile://" (.getAbsolutePath (io/file path)) "::" entry)
    :else (str "jar:" (path->uri path) "!/" entry)))

(defn position
  "clj-kondo's `row` and `col` as an LSP position."
  [row col]
  {:line (dec row) :character (dec col)})

(defn range
  "clj-kondo's [row col end-row end-col] as an LSP range."
  [[row col end-row end-col]]
  {:start (position row col) :end (position end-row end-col)})

(defn ->kondo
  "An LSP position as clj-kondo's [row col]."
  [{:keys [line character]}]
  [(inc line) (inc character)])

(defn location
  "A clojure-lite-lsp location ({:path :entry :pos}) as an LSP Location."
  [{:keys [pos] :as loc} opts]
  {:uri (location-uri loc opts)
   ;; without a position (a Java class's file): its start
   :range (range (if (and (seq pos) (every? some? pos)) pos [1 1 1 1]))})
