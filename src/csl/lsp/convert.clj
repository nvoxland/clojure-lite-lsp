(ns csl.lsp.convert
  "Between csl's positions and paths and LSP's.

  clj-kondo's positions are 1-based rows and columns, end exclusive, with
  columns in UTF-16 code units (checked: an emoji counts as two), which is
  LSP's default position encoding. So converting is subtracting one.

  Files inside jars get `jar:file:///x.jar!/entry` URIs, or for clients
  that open those, `zipfile:///x.jar::entry`."
  (:refer-clojure :exclude [range])
  (:require
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
  [uri]
  (when (and uri (str/starts-with? uri "file:"))
    (.getPath (URI. (str/replace uri #"^file:(?!//)" "file://")))))

(defn location-uri
  "The URI of a location's file: a file, or {:path jar :entry} inside a jar."
  [{:keys [path entry]} {:keys [dependency-scheme]}]
  (cond
    (nil? entry) (path->uri path)
    (= "zipfile" dependency-scheme) (str "zipfile://" (uri->path (path->uri path)) "::" entry)
    :else (str "jar:" (path->uri path) "!/" entry)))

(defn uri->location
  "A URI as {:path} for a file, or {:path jar :entry} inside a jar."
  [uri]
  (cond
    (str/starts-with? uri "jar:")
    (let [[_ jar entry] (re-matches #"jar:(.*)!/(.*)" uri)]
      {:path (uri->path jar) :entry entry})

    (str/starts-with? uri "zipfile:")
    (let [[_ jar entry] (re-matches #"zipfile://(.*)::(.*)" uri)]
      {:path jar :entry entry})

    :else {:path (uri->path uri)}))

(defn position [row col] {:line (dec row) :character (dec col)})

(defn range [[row col end-row end-col]]
  {:start (position row col) :end (position end-row end-col)})

(defn ->kondo
  "An LSP position as clj-kondo's [row col]."
  [{:keys [line character]}]
  [(inc line) (inc character)])

(defn location
  "A csl location ({:path :entry :pos}) as an LSP Location."
  [{:keys [pos] :as loc} opts]
  {:uri (location-uri loc opts)
   :range (if (every? some? pos) (range pos) (range [1 1 1 1]))})
