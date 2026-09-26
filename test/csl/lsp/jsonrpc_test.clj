(ns csl.lsp.jsonrpc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [csl.lsp.jsonrpc :as rpc])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn round-trip [& msgs]
  (let [out (ByteArrayOutputStream.)]
    (doseq [m msgs] (rpc/write-message! out m))
    (let [in (ByteArrayInputStream. (.toByteArray out))]
      (vec (take-while some? (repeatedly #(rpc/read-message in)))))))

(deftest frames-messages
  (testing "Content-Length counts bytes, not characters"
    (is (= [{:jsonrpc "2.0" :id 1 :result {:text "é 😀 ü"}}]
           (round-trip {:jsonrpc "2.0" :id 1 :result {:text "é 😀 ü"}}))))
  (testing "several messages back to back, then end of input"
    (is (= [{:id 1} {:id 2 :method "x"}] (round-trip {:id 1} {:id 2 :method "x"})))))

(deftest tolerates-extra-headers
  (let [body "{\"id\":7}"
        raw (str "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: "
                 (count body) "\r\n\r\n" body)]
    (is (= {:id 7} (rpc/read-message (ByteArrayInputStream. (.getBytes raw "UTF-8")))))))

(deftest end-of-input-is-nil
  (is (nil? (rpc/read-message (ByteArrayInputStream. (byte-array 0))))))
