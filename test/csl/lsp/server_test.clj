(ns csl.lsp.server-test
  "End to end: LSP over streams to the real server, with an in-process
  daemon, on a real indexed project."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [csl.classpath-test :refer [project!]]
   [csl.daemon :as daemon]
   [csl.lsp.convert :as convert]
   [csl.lsp.jsonrpc :as rpc]
   [csl.lsp.server :as server]
   [csl.test-util :as tu])
  (:import
   [java.io PipedInputStream PipedOutputStream]))

(defn start!
  "Start a server on piped streams. Returns a client: {:send! :request!
  :notify! :server :home}."
  [home]
  (let [to-server (PipedOutputStream.)
        server-in (PipedInputStream. to-server (* 1024 1024))
        from-server (PipedOutputStream.)
        client-in (PipedInputStream. from-server (* 1024 1024))
        spawn! #(future (daemon/run! {:home home :poll-ms 20 :version "test"}))
        srv (future (server/run! {:in server-in :out from-server :home home :version "test" :spawn! spawn!}))
        ids (atom 0)
        responses (atom {})
        notifications (atom [])
        reader (future
                 (loop []
                   (when-let [msg (rpc/read-message client-in)]
                     (cond
                       ;; a request from the server (registerCapability, progress): accept
                       (and (:method msg) (:id msg)) (do (swap! notifications conj msg)
                                                         (rpc/write-message! to-server {:jsonrpc "2.0" :id (:id msg) :result nil}))
                       (:id msg) (swap! responses assoc (:id msg) msg)
                       :else (swap! notifications conj msg))
                     (recur))))]
    {:server srv :reader reader :home home :notifications notifications
     :notify! (fn [method params] (rpc/write-message! to-server {:jsonrpc "2.0" :method method :params params}))
     :request! (fn [method params]
                 (let [id (swap! ids inc)]
                   (rpc/write-message! to-server {:jsonrpc "2.0" :id id :method method :params params})
                   (loop [n 0]
                     (if-let [r (@responses id)]
                       (if (:error r) (throw (ex-info (str "LSP error: " (:error r)) r)) (:result r))
                       (if (< n 3000) (do (Thread/sleep 10) (recur (inc n))) (throw (ex-info "No response" {:method method})))))))}))

(defn wait-indexed!
  "Until the queue has stayed empty for a moment."
  [{:keys [request!]}]
  (loop [quiet 0 n 0]
    (when (and (< quiet 5) (< n 3000))
      (Thread/sleep 50)
      (recur (if (zero? (:pending (request! "csl/status" {}))) (inc quiet) 0) (inc n)))))

(defn uri [root rel] (convert/path->uri (str root "/" rel)))

(defn pos-of
  "The LSP position of the first character of `needle` in `text`."
  [text needle]
  (let [i (str/index-of text needle)
        before (subs text 0 i)]
    {:line (count (re-seq #"\n" before))
     :character (- i (inc (or (str/last-index-of before "\n") -1)))}))

(defn at [root rel needle]
  {:textDocument {:uri (uri root rel)} :position (pos-of (slurp (io/file root rel)) needle)})

(deftest an-editor-session
  (let [a "(ns app.a\n  \"The a namespace.\")\n\n(defn greet\n  \"Says hello.\"\n  [who]\n  (str \"hello \" who))\n"
        b "(ns app.b\n  (:require [app.a :as a]))\n\n(defn main []\n  (a/greet \"you\"))\n"
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" a "src/app/b.clj" b})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
    (testing "initialize advertises the reading features"
      (let [caps (:capabilities (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}}))]
        (is (every? caps [:definitionProvider :referencesProvider :hoverProvider :implementationProvider
                          :documentSymbolProvider :workspaceSymbolProvider :callHierarchyProvider]))))
    (notify! "initialized" {})
    (wait-indexed! client)
    (notify! "textDocument/didOpen" {:textDocument {:uri (uri root "src/app/b.clj") :languageId "clojure" :version 1 :text b}})

    (testing "definition across files"
      (is (= [{:uri (uri root "src/app/a.clj") :range {:start {:line 3 :character 6} :end {:line 3 :character 11}}}]
             (request! "textDocument/definition" (at root "src/app/b.clj" "greet")))))

    (testing "references"
      (is (= [(uri root "src/app/b.clj")]
             (map :uri (request! "textDocument/references" (assoc (at root "src/app/a.clj" "greet")
                                                                  :context {:includeDeclaration false}))))))

    (testing "hover shows the docstring and arglists"
      (let [text (get-in (request! "textDocument/hover" (at root "src/app/b.clj" "greet")) [:contents :value])]
        (is (str/includes? text "app.a/greet"))
        (is (str/includes? text "[who]"))
        (is (str/includes? text "Says hello."))))

    (testing "document symbols: the namespace, with its definitions inside"
      (let [[ns-sym] (request! "textDocument/documentSymbol" {:textDocument {:uri (uri root "src/app/a.clj")}})]
        (is (= "app.a" (:name ns-sym)))
        (is (= ["greet"] (map :name (:children ns-sym))))))

    (testing "workspace symbols"
      (is (= [["greet" "app.a"]]
             (map (juxt :name :containerName)
                  (filter #(= "app.a" (:containerName %)) (request! "workspace/symbol" {:query "gree"}))))))

    (testing "call hierarchy"
      (let [[item] (request! "textDocument/prepareCallHierarchy" (at root "src/app/a.clj" "greet"))]
        (is (= "greet" (:name item)))
        (is (= ["main"] (map (comp :name :from) (request! "callHierarchy/incomingCalls" {:item item}))))))

    (testing "an unsaved edit above the cursor still resolves"
      (let [edited (str ";; a comment\n;; and another\n" b)]
        (notify! "textDocument/didChange" {:textDocument {:uri (uri root "src/app/b.clj") :version 2}
                                           :contentChanges [{:text edited}]})
        (is (= [(uri root "src/app/a.clj")]
               (map :uri (request! "textDocument/definition"
                                   {:textDocument {:uri (uri root "src/app/b.clj")}
                                    :position (pos-of edited "greet")}))))
        (testing "and results in the edited file come back in buffer positions"
          (is (= [(:line (pos-of edited "(a/greet"))]
                 (map (comp :line :start :range)
                      (request! "textDocument/references" (assoc (at root "src/app/a.clj" "greet")
                                                                 :context {:includeDeclaration false}))))))
        (notify! "textDocument/didChange" {:textDocument {:uri (uri root "src/app/b.clj") :version 3}
                                           :contentChanges [{:text b}]})))

    (testing "a save is re-indexed"
      (let [a2 (str/replace a "greet" "welcome")]
        (spit (io/file root "src/app/a.clj") a2)
        (notify! "textDocument/didSave" {:textDocument {:uri (uri root "src/app/a.clj")}})
        (wait-indexed! client)
        (is (seq (filter #(= "welcome" (:name %)) (request! "workspace/symbol" {:query "welcome"}))))))

    (testing "a file created outside the editor, reported by the watcher"
      (spit (io/file root "src/app/c.clj") "(ns app.c)\n(defn created [] 1)\n")
      (notify! "workspace/didChangeWatchedFiles" {:changes [{:uri (uri root "src/app/c.clj") :type 1}]})
      (wait-indexed! client)
      (is (seq (filter #(= "created" (:name %)) (request! "workspace/symbol" {:query "created"})))))

    (testing "shutdown and exit"
      (is (nil? (request! "shutdown" nil)))
      (notify! "exit" nil)
      (is (= 0 (deref (:server client) 10000 :timeout))))))

(deftest indexing-progress
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a) (defn f [] 1)"})
        {:keys [request! notify! notifications] :as client} (start! (str (tu/temp-dir)))]
    (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {:window {:workDoneProgress true}}})
    (notify! "initialized" {})
    (wait-indexed! client)
    (Thread/sleep 1500)
    (let [kinds (->> @notifications (filter #(= "$/progress" (:method %))) (map (comp :kind :value :params)))]
      (is (some #{"window/workDoneProgress/create"} (map :method @notifications)))
      (is (= "begin" (first kinds)))
      (is (= "end" (last kinds))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest requests-inside-jar-files-answer-nothing-yet
  ;; library files opened from a definition have jar: URIs; until they get
  ;; full analysis, requests there answer empty rather than failing
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        in-jar {:textDocument {:uri "jar:file:///m2/clojure.jar!/clojure/core.clj"} :position {:line 10 :character 3}}]
    (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}})
    (notify! "textDocument/didOpen" {:textDocument {:uri "jar:file:///m2/clojure.jar!/clojure/core.clj" :text "(ns clojure.core)"}})
    (is (= [] (request! "textDocument/definition" in-jar)))
    (is (= [] (request! "textDocument/references" (assoc in-jar :context {:includeDeclaration true}))))
    (is (nil? (request! "textDocument/hover" in-jar)))
    (is (= [] (request! "textDocument/documentSymbol" {:textDocument (:textDocument in-jar)})))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest navigating-into-library-code
  ;; a definition in a jar comes back as an extracted, read-only file
  ;; (every editor opens file:// URIs); opened, it gets full analysis, so
  ;; navigation continues inside it
  (let [home (str (tu/temp-dir))
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)\n(defn f [xs] (keep identity xs))\n"})
        {:keys [request! notify!] :as client} (start! home)]
    (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}})
    (notify! "initialized" {})
    (wait-indexed! client)
    (let [[{:keys [uri range]}] (request! "textDocument/definition" (at root "src/app/a.clj" "keep"))
          core (convert/uri->path uri)
          core-text (slurp core)]
      (is (str/starts-with? core (str (.getCanonicalPath (io/file home)) "/sources/")))
      (is (str/ends-with? core "/clojure/core.clj"))
      (is (str/includes? (nth (str/split-lines core-text) (get-in range [:start :line])) "keep"))
      (testing "opened, the library file is analyzed fully"
        (notify! "textDocument/didOpen" {:textDocument {:uri uri :languageId "clojure" :version 1 :text core-text}})
        (wait-indexed! client)
        (let [keep-line (get-in range [:start :line])
              body (str/join "\n" (drop keep-line (str/split-lines core-text)))
              ;; a call to lazy-seq inside keep's body
              {:keys [line character]} (pos-of body "lazy-seq")
              [target] (request! "textDocument/definition" {:textDocument {:uri uri}
                                                            :position {:line (+ keep-line line) :character character}})]
          (is (= uri (:uri target)) "lazy-seq is defined in clojure/core.clj too")
          (is (str/includes? (nth (str/split-lines core-text) (get-in target [:range :start :line])) "lazy-seq"))
          (is (seq (request! "textDocument/documentSymbol" {:textDocument {:uri uri}}))))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest navigating-to-java-sources
  (when (csl.java/jdk-src)
    (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a (:import [java.io File]))\n(defn f [] (File. \"x\"))\n"})
          {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
      (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}})
      (notify! "initialized" {})
      (wait-indexed! client)
      (let [[{:keys [uri range]}] (request! "textDocument/definition" (at root "src/app/a.clj" "File. "))
            path (convert/uri->path uri)]
        (is (str/ends-with? path "/java/io/File.java"))
        (is (str/includes? (nth (str/split-lines (slurp path)) (get-in range [:start :line])) "class File")))
      (request! "shutdown" nil)
      (notify! "exit" nil)
      (deref (:server client) 10000 :timeout))))

(deftest two-editors-on-one-project
  ;; two csl lsp processes, one project: one daemon, both answer
  (let [home (str (tu/temp-dir))
        root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a)\n(defn f [] 1)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/f)\n"})
        daemons (atom 0)
        real-run daemon/run!]
    (with-redefs [daemon/run! (fn [opts] (swap! daemons inc) (real-run opts))]
      (let [editors [(start! home) (start! home)]]
        (doseq [{:keys [request! notify!]} editors]
          (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}})
          (notify! "initialized" {}))
        (wait-indexed! (first editors))
        (doseq [{:keys [request!]} editors]
          (is (= [(uri root "src/app/a.clj")]
                 (map :uri (request! "textDocument/definition" (at root "src/app/b.clj" "a/f"))))))
        (is (= 1 @daemons))
        (doseq [{:keys [request! notify! server]} editors]
          (request! "shutdown" nil)
          (notify! "exit" nil)
          (deref server 10000 :timeout))))))

(deftest a-project-opened-through-a-symlink
  ;; editors send paths as the user opened them; the index has canonical ones
  (let [real (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a)\n(defn f [] 1)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/f)\n"})
        link (str (tu/temp-dir) "/linked-project")
        _ (java.nio.file.Files/createSymbolicLink (.toPath (io/file link)) (.toPath (io/file real))
                                                  (make-array java.nio.file.attribute.FileAttribute 0))
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
    (request! "initialize" {:rootUri (convert/path->uri link) :capabilities {}})
    (notify! "initialized" {})
    (wait-indexed! client)
    (let [b (slurp (io/file link "src/app/b.clj"))]
      (notify! "textDocument/didOpen" {:textDocument {:uri (uri link "src/app/b.clj") :languageId "clojure" :version 1 :text b}})
      (is (= ["a.clj"] (map #(.getName (io/file (convert/uri->path (:uri %))))
                            (request! "textDocument/definition" {:textDocument {:uri (uri link "src/app/b.clj")}
                                                                 :position (pos-of b "a/f")})))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))
