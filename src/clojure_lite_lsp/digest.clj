(ns clojure-lite-lsp.digest
  "SHA-256 content hashes, and their hex form where they name files."
  (:require
   [clojure.java.io :as io]
   [clojure.walk :as walk])
  (:import
   [java.io File InputStream]
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]
   [java.util HexFormat]))

(set! *warn-on-reflection* true)

(defprotocol ^:private Digestible
  (add! [x md] "Feed `x` to MessageDigest `md`."))

(extend-protocol Digestible
  byte/1
  (add! [bs md] (.update ^MessageDigest md ^bytes bs))

  String
  (add! [s md] (.update ^MessageDigest md (.getBytes s StandardCharsets/UTF_8)))

  InputStream
  (add! [in md]
    (let [buf (byte-array 65536)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.update ^MessageDigest md buf 0 n)
            (recur))))))

  File
  (add! [f md]
    (with-open [in (io/input-stream f)]
      (add! in md))))

(defn sha256
  "The SHA-256 of `parts` one after the other: byte arrays, strings (as
  UTF-8), files and input streams (read to the end)."
  ^bytes [& parts]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (doseq [p parts] (add! p md))
    (.digest md)))

(defn hex
  "`bs` as lower-case hex."
  ^String [^bytes bs]
  (.formatHex (HexFormat/of) bs))

(defn unhex
  "The bytes a `hex` string stands for."
  ^bytes [^String s]
  (.parseHex (HexFormat/of) s))

(defn- canonical
  "A form whose printed text is the same for equal values: maps and sets
  sorted."
  [x]
  (walk/postwalk (fn [v]
                   (cond
                     (map? v) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) v)
                     (set? v) (vec (sort-by pr-str v))
                     :else v))
                 x))

(defn value-hex
  "The SHA-256 of value `x`, as hex: the same for equal values."
  [x]
  (hex (sha256 (binding [*print-length* nil *print-level* nil] (pr-str (canonical x))))))
