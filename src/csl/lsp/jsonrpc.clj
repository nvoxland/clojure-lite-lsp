(ns csl.lsp.jsonrpc
  "JSON-RPC 2.0 messages framed with LSP's Content-Length headers."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str])
  (:import
   [java.io ByteArrayOutputStream InputStream OutputStream]
   [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(defn- read-line-ascii
  "A header line (without CRLF), or nil at end of input."
  [^InputStream in]
  (let [buf (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond
          (neg? b) (when (pos? (.size buf)) (.toString buf "US-ASCII"))
          (= b 10) (str/trimr (.toString buf "US-ASCII"))
          :else (do (.write buf b) (recur)))))))

(defn read-message
  "The next message from `in` as a map with keyword keys, or nil at end of
  input. A body that isn't JSON is {::parse-error message}, so the caller
  can answer it and go on; a header block without a length is skipped."
  [^InputStream in]
  (loop [length nil]
    (when-let [line (read-line-ascii in)]
      (if (str/blank? line)
        (if length
          (let [body (.readNBytes in (int length))]
            (when (= length (alength body))
              (try (json/parse-string (String. body StandardCharsets/UTF_8) true)
                   (catch Exception e {::parse-error (ex-message e)}))))
          (recur nil))
        (recur (if-let [[_ n] (re-matches #"(?i)content-length:\s*(\d+)" line)]
                 (parse-long n)
                 length))))))

(defn write-message!
  "Write `msg` to `out`. Safe to call from several threads."
  [^OutputStream out msg]
  (let [body (.getBytes ^String (json/generate-string msg) StandardCharsets/UTF_8)]
    (locking out
      (.write out (.getBytes (str "Content-Length: " (alength body) "\r\n\r\n") StandardCharsets/US_ASCII))
      (.write out body)
      (.flush out))))
