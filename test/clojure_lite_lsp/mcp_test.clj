(ns clojure-lite-lsp.mcp-test
  "The MCP server: the query commands as tools, for agents that speak MCP
  (Codex) rather than LSP."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.cli-test :refer [opts stop!]]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.daemon-test :refer [home project!]]
   [clojure-lite-lsp.mcp :as mcp]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn session
  "Run the server over `messages` (newline-delimited JSON-RPC, as MCP's
  stdio transport has it): its responses."
  [o cwd messages]
  (let [out (ByteArrayOutputStream.)]
    (mcp/run! {:in (ByteArrayInputStream. (.getBytes (str (str/join "\n" (map json/generate-string messages)) "\n") "UTF-8"))
               :out out :opts o :cwd cwd})
    (mapv #(json/parse-string % true) (str/split-lines (.toString out "UTF-8")))))

(deftest tools-for-agents
  (let [h (home)
        daemons (atom [])
        root (project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})
        [init tools call bad]
        (session (opts h daemons) root
                 [{:jsonrpc "2.0" :id 1 :method "initialize"
                   :params {:protocolVersion "2025-06-18" :capabilities {} :clientInfo {:name "test" :version "1"}}}
                  {:jsonrpc "2.0" :method "notifications/initialized"}
                  {:jsonrpc "2.0" :id 2 :method "tools/list"}
                  {:jsonrpc "2.0" :id 3 :method "tools/call" :params {:name "references" :arguments {:target "app.a/greet"}}}
                  {:jsonrpc "2.0" :id 4 :method "tools/call" :params {:name "references" :arguments {}}}])]
    (try
      (testing "initialize: the client's protocol version, and tools"
        (is (= "2025-06-18" (get-in init [:result :protocolVersion])))
        (is (contains? (get-in init [:result :capabilities]) :tools))
        (is (= "clojure-lite-lsp" (get-in init [:result :serverInfo :name]))))
      (testing "every query command is a tool"
        (is (= #{"definition" "references" "implementations" "doc" "callers" "callees" "symbols" "outline"}
               (set (map :name (get-in tools [:result :tools])))))
        (is (every? #(get-in % [:inputSchema :properties]) (get-in tools [:result :tools]))))
      (testing "a call answers as the CLI does"
        (is (= [{:type "text" :text "src/app/b.clj:2:2: (a/greet 1)"}] (get-in call [:result :content])))
        (is (false? (get-in call [:result :isError]))))
      (testing "a bad call is an error result, not a crash"
        (is (true? (get-in bad [:result :isError]))))
      (finally (stop! h daemons)))))

(deftest protocol-details
  (let [h (home)
        daemons (atom [])
        root (project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n(greet 1) (greet 2)\n"})
        o (opts h daemons)]
    (try
      (let [[ping unknown batch limited]
            (session o root
                     [{:jsonrpc "2.0" :id 1 :method "ping"}
                      {:jsonrpc "2.0" :id 2 :method "resources/list"}
                      [{:jsonrpc "2.0" :id 3 :method "ping"} {:jsonrpc "2.0" :id 4 :method "ping"}]
                      {:jsonrpc "2.0" :id 5 :method "tools/call" :params {:name "references" :arguments {:target "app.a/greet" :limit 1}}}])]
        (is (= {} (:result ping)))
        (is (= -32601 (get-in unknown [:error :code])))
        (is (= [3 4] (map :id batch)) "a batch is answered as a batch")
        (is (clojure.string/includes? (get-in limited [:result :content 0 :text]) "1 more")))
      (testing "any failure is a tool error, not a protocol one"
        (with-redefs [commands/run (fn [& _] (throw (RuntimeException. "boom")))]
          (let [[r] (session o root [{:jsonrpc "2.0" :id 1 :method "tools/call" :params {:name "definition" :arguments {:target "a/b"}}}])]
            (is (true? (get-in r [:result :isError])))
            (is (clojure.string/includes? (get-in r [:result :content 0 :text]) "boom")))))
      (testing "a call doesn't wait past its deadline for indexing: it answers, and says so"
        (with-redefs [cli/index! (fn [_ dirs _ & [{:keys [deadline-ms]}]]
                                                    (is deadline-ms)
                                                    {:files {} :pending {(first dirs) 7}})]
          (let [[r] (session o root [{:jsonrpc "2.0" :id 1 :method "tools/call" :params {:name "definition" :arguments {:target "app.a/greet"}}}])]
            (is (clojure.string/includes? (get-in r [:result :content 0 :text]) "still indexing")))))
      (finally (stop! h daemons)))))
