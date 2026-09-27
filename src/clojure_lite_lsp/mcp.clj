(ns clojure-lite-lsp.mcp
  "`clojure-lite-lsp mcp`: an MCP server offering the query commands
  (clojure-lite-lsp.commands) as tools, for agents that speak MCP rather
  than LSP (Codex). MCP's stdio transport: one JSON-RPC message a line.

  Each call runs as `clojure-lite-lsp query` would, in the server's working
  directory or the call's `project`: the project's index is brought up to
  date first, so what the agent just edited is answered."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.cli :as cli]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.version :as version])
  (:import
   [java.io BufferedReader InputStream OutputStream]
   [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(def ^:private protocol-versions
  "The MCP versions this server speaks, newest last."
  ["2024-11-05" "2025-03-26" "2025-06-18"])

(def ^:private instructions
  (str "Code navigation for Clojure from a static index: definitions, references, callers, "
       "callees, implementations, docs, symbol search and file outlines. A target is a symbol "
       "(ns/name for a var, ns for a namespace) or a position file:line:col (1-based). "
       "Results are path:line:col lines, paths relative to the project root."))

(defn- tool [{:keys [name usage doc]}]
  {:name name
   :description (str doc " Argument: " usage ".")
   :inputSchema {:type "object"
                 :properties {:target {:type "string"
                                       :description (if (= "<symbol | file:line:col>" usage)
                                                      "ns/name (a var), ns (a namespace), or file:line:col (1-based)"
                                                      (subs usage 1 (dec (count usage))))}
                              :project {:type "string"
                                        :description "The project directory (default: the server's working directory)"}}
                 :required ["target"]}})

(defn- call [{:keys [opts cwd]} {:keys [name arguments]}]
  (let [{:keys [target project]} arguments
        {:keys [exit out]} (cli/query! opts (cond-> [name (str target)] project (conj "--project" project)) {:cwd cwd})]
    {:content [{:type "text" :text out}] :isError (not= 0 exit)}))

(defn- respond [ctx {:keys [method params]}]
  (case method
    "initialize" {:protocolVersion (let [v (:protocolVersion params)]
                                     (if (some #{v} protocol-versions) v (peek protocol-versions)))
                  :capabilities {:tools {}}
                  :serverInfo {:name "clojure-lite-lsp" :version version/version}
                  :instructions instructions}
    "ping" {}
    "tools/list" {:tools (mapv tool commands/commands)}
    "tools/call" (call ctx params)
    ::unknown))

(defn run!
  "Serve MCP on `in`/`out` until the input ends. `opts` are
  clojure-lite-lsp.client/ensure-daemon!'s; `cwd` the default project dir."
  [{:keys [^InputStream in ^OutputStream out] :as ctx}]
  (let [reader (BufferedReader. (io/reader in :encoding "UTF-8"))
        send! (fn [msg]
                (.write out (.getBytes (str (json/generate-string msg) "\n") StandardCharsets/UTF_8))
                (.flush out))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (let [msg (try (json/parse-string line true) (catch Exception _ ::bad))]
            (cond
              (= ::bad msg) (send! {:jsonrpc "2.0" :id nil :error {:code -32700 :message "Parse error"}})
              ;; notifications (initialized, cancelled): nothing to answer
              (nil? (:id msg)) nil
              :else
              (let [result (try (respond ctx msg)
                                (catch Exception e {::error (ex-message e)}))]
                (send! (merge {:jsonrpc "2.0" :id (:id msg)}
                              (cond
                                (= ::unknown result) {:error {:code -32601 :message (str "Unsupported: " (:method msg))}}
                                (::error result) {:error {:code -32603 :message (::error result)}}
                                :else {:result result})))))))
        (recur)))))
