(ns clojure-lite-lsp.mcp-test
  "The MCP server: the query commands as tools, for agents that speak MCP
  (Codex) rather than LSP."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.daemon-fixture :refer [with-in-process-daemons]]
   [clojure-lite-lsp.mcp :as mcp]
   [clojure-lite-lsp.test-util :refer [build-free-project!]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn session
  "Run the server over `messages` (newline-delimited JSON-RPC, as MCP's
  stdio transport has it): its responses."
  [o cwd messages]
  (let [out (ByteArrayOutputStream.)]
    (mcp/serve! {:in (ByteArrayInputStream. (.getBytes (str (str/join "\n" (map json/generate-string messages)) "\n") "UTF-8"))
                 :out out :opts o :cwd cwd})
    (mapv #(json/parse-string % true) (str/split-lines (.toString out "UTF-8")))))

(deftest the-query-commands-are-tools
  (with-in-process-daemons [o _ _]
    (let [root (build-free-project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n"
                                     "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})
          [init tools call bad]
          (session o root
                   [{:jsonrpc "2.0" :id 1 :method "initialize"
                     :params {:protocolVersion "2025-06-18" :capabilities {} :clientInfo {:name "test" :version "1"}}}
                    {:jsonrpc "2.0" :method "notifications/initialized"}
                    {:jsonrpc "2.0" :id 2 :method "tools/list"}
                    {:jsonrpc "2.0" :id 3 :method "tools/call" :params {:name "references" :arguments {:target "app.a/greet"}}}
                    {:jsonrpc "2.0" :id 4 :method "tools/call" :params {:name "references" :arguments {}}}])]
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
        (is (true? (get-in bad [:result :isError])))))))

(defn- tool-call [id name arguments]
  {:jsonrpc "2.0" :id id :method "tools/call" :params {:name name :arguments arguments}})

(deftest the-protocols-other-requests
  (with-in-process-daemons [o _ _]
    (let [root (build-free-project! {"src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n(greet 1) (greet 2)\n"})
          [ping unknown batch limited]
          (session o root
                   [{:jsonrpc "2.0" :id 1 :method "ping"}
                    {:jsonrpc "2.0" :id 2 :method "resources/list"}
                    [{:jsonrpc "2.0" :id 3 :method "ping"} {:jsonrpc "2.0" :id 4 :method "ping"}]
                    (tool-call 5 "references" {:target "app.a/greet" :limit 1})])]
      (testing "ping"
        (is (= {} (:result ping))))
      (testing "an unknown method"
        (is (= -32601 (get-in unknown [:error :code]))))
      (testing "a batch is answered as a batch"
        (is (= [3 4] (map :id batch))))
      (testing "a limit, and how many more there are"
        (is (str/includes? (get-in limited [:result :content 0 :text]) "1 more")))
      (testing "any failure is a tool error, not a protocol one"
        (with-redefs [commands/run (fn [& _] (throw (RuntimeException. "boom")))]
          (let [[r] (session o root [(tool-call 1 "definition" {:target "a/b"})])]
            (is (true? (get-in r [:result :isError])))
            (is (str/includes? (get-in r [:result :content 0 :text]) "boom")))))
      (testing "a call doesn't wait past its deadline for indexing: it answers, and says so"
        (with-redefs [cli/index! (fn [_ dirs _ {:keys [deadline-ms]}]
                                   (is deadline-ms "the call gives indexing a deadline")
                                   {:files {} :pending {(first dirs) 7}})]
          (let [[r] (session o root [(tool-call 1 "definition" {:target "app.a/greet"})])]
            (is (str/includes? (get-in r [:result :content 0 :text]) "still indexing"))))))))
