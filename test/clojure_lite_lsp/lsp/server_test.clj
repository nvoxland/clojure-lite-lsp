(ns clojure-lite-lsp.lsp.server-test
  "End to end: LSP over streams to the real server, with an in-process
  daemon, on a real indexed project."
  (:require
   [clojure-lite-lsp.classpath-test :refer [project!]]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.java :as java]
   [clojure-lite-lsp.lsp.convert :as convert]
   [clojure-lite-lsp.lsp.jsonrpc :as rpc]
   [clojure-lite-lsp.lsp.server :as server]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.sources :as sources]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
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
        spawn! #(future (daemon/serve! {:home home :poll-ms 20 :version "test"}))
        srv (future (server/serve! {:in server-in :out from-server :home home :version "test" :spawn! spawn!}))
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
      (recur (if (zero? (:pending (request! "clojure-lite-lsp/status" {}))) (inc quiet) 0) (inc n)))))

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
                          :documentSymbolProvider :workspaceSymbolProvider :callHierarchyProvider]))
        (testing "and nothing that edits: editors don't offer rename"
          (is (not-any? caps [:renameProvider :codeActionProvider :documentFormattingProvider])))))
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
  (when (java/jdk-src)
    (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a (:import [java.io File]))\n(defn f [] (File. \"x\"))\n"})
          {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
      (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {}})
      (notify! "initialized" {})
      (wait-indexed! client)
      (let [[{:keys [uri range]}] (request! "textDocument/definition" (at root "src/app/a.clj" "File. "))
            path (convert/uri->path uri)]
        (is (str/ends-with? path "/java/io/File.java"))
        (is (str/includes? (nth (str/split-lines (slurp path)) (get-in range [:start :line])) "class File")))
      (testing "hover names the class"
        (is (str/includes? (get-in (request! "textDocument/hover" (at root "src/app/a.clj" "File. ")) [:contents :value])
                           "java.io.File")))
      (request! "shutdown" nil)
      (notify! "exit" nil)
      (deref (:server client) 10000 :timeout))))

(deftest two-editors-on-one-project
  ;; two clojure-lite-lsp lsp processes, one project: one daemon, both answer
  (let [home (str (tu/temp-dir))
        root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a)\n(defn f [] 1)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/f)\n"})
        daemons (atom 0)
        real-run daemon/serve!]
    (with-redefs [daemon/serve! (fn [opts] (swap! daemons inc) (real-run opts))]
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

(deftest one-failed-change-doesnt-drop-the-others
  ;; a git checkout reports many files at once
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        real queue/enqueue!]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (spit (io/file root "src/app/x.clj") "(ns app.x)\n(defn lost [] 1)\n")
    (spit (io/file root "src/app/y.clj") "(ns app.y)\n(defn kept [] 1)\n")
    (with-redefs [queue/enqueue! (fn [c p kind path priority]
                                       (if (str/ends-with? path "x.clj")
                                         (throw (ex-info "database is busy" {}))
                                         (real c p kind path priority)))]
      (notify! "workspace/didChangeWatchedFiles" {:changes [{:uri (uri root "src/app/x.clj") :type 1}
                                                            {:uri (uri root "src/app/y.clj") :type 1}]})
      (wait-indexed! client))
    (is (seq (filter #(= "kept" (:name %)) (request! "workspace/symbol" {:query "kept"}))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(defn frame [^String body]
  (let [b (.getBytes body "UTF-8")]
    (str "Content-Length: " (alength b) "\r\n\r\n" body)))

(defn serve-bytes
  "Run the server over `input` (framed messages) to its end: [exit-code
  responses]."
  [input]
  (let [out (java.io.ByteArrayOutputStream.)
        code (server/serve! {:in (java.io.ByteArrayInputStream. (.getBytes ^String input "UTF-8")) :out out
                           :home (str (tu/temp-dir)) :version "test"})
        in (java.io.ByteArrayInputStream. (.toByteArray out))]
    [code (vec (take-while some? (repeatedly #(rpc/read-message in))))]))

(deftest bad-input-is-answered-not-fatal
  (let [[code responses] (serve-bytes (str (frame "{not json")
                                           (frame "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}")
                                           (frame "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}")))]
    (is (= -32700 (get-in (first responses) [:error :code])))
    (is (= 0 code) "and it carries on")))

(deftest requests-after-shutdown-are-refused
  (let [[code responses] (serve-bytes (str (frame "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}")
                                           (frame "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"workspace/symbol\",\"params\":{\"query\":\"x\"}}")
                                           (frame "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}")))]
    (is (= -32600 (get-in (second responses) [:error :code])))
    (is (= 0 code))))

(deftest symbol-search-doesnt-wait-for-extraction
  ;; each library hit would be a jar opened and a file written, per
  ;; keystroke: the files are extracted in the background instead
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" "(ns app.a)"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        real sources/extract!]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (with-redefs [sources/extract! (fn [home loc] (Thread/sleep 1000) (real home loc))]
      (let [started (System/currentTimeMillis)
            hits (request! "workspace/symbol" {:query "mapcat"})]
        (is (< (- (System/currentTimeMillis) started) 900))
        (let [f (io/file (convert/uri->path (get-in (first (filter #(= "mapcat" (:name %)) hits)) [:location :uri])))]
          (is (loop [n 0] (or (.isFile f) (when (< n 100) (Thread/sleep 50) (recur (inc n)))))
              "the file is there soon after"))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest agents-can-have-requests-wait-for-the-index
  ;; an editor is served from whatever is indexed so far; an agent asks
  ;; once and trusts the answer, so it can ask to wait (waitForIndex)
  (let [root (project! {"deps.edn" "{:paths [\"src\"]}"
                        "src/app/a.clj" "(ns app.a)\n(defn greet [who] who)\n"
                        "src/app/b.clj" "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
    (request! "initialize" {:rootUri (convert/path->uri root) :initializationOptions {:waitForIndex true}})
    (notify! "initialized" {})
    ;; asked at once: the first index is still running
    (is (= [(uri root "src/app/a.clj")]
           (map :uri (request! "textDocument/definition" (at root "src/app/b.clj" "greet")))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(defn end-of [text] (let [lines (str/split text #"\n" -1)] {:line (dec (count lines)) :character (count (last lines))}))

(deftest editing-helpers
  (let [a "(ns app.a)\n(defn greet\n  \"Says hello.\"\n  [who]\n  (str who who))\n(defn twice [x] (greet x) (greet x))\n"
        b "(ns app.b (:require [app.a :as a]))\n(a/greet 1)\n"
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" a "src/app/b.clj" b})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        doc {:uri (uri root "src/app/a.clj")}]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (notify! "textDocument/didOpen" {:textDocument (assoc doc :languageId "clojure" :version 1 :text a)})
    (testing "highlights: definitions write, uses read"
      (is (= #{[(pos-of a "greet\n") 3] [(pos-of a "greet x") 2] [(pos-of a "greet x))") 2]}
             (set (map (juxt (comp :start :range) :kind)
                       (request! "textDocument/documentHighlight" (at root "src/app/a.clj" "greet x"))))))
      (is (= 3 (count (request! "textDocument/documentHighlight" (at root "src/app/a.clj" "who who"))))))
    (testing "rename is unsupported"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                            (request! "textDocument/rename" (assoc (at root "src/app/a.clj" "who who") :newName "person")))))
    (testing "signature help while typing a new call"
      (let [typed (str a "(greet ")]
        (notify! "textDocument/didChange" {:textDocument (assoc doc :version 2) :contentChanges [{:text typed}]})
        (let [{:keys [signatures activeParameter]} (request! "textDocument/signatureHelp" {:textDocument doc :position (end-of typed)})]
          (is (= ["greet [who]"] (map :label signatures)))
          (is (= 0 activeParameter))
          (is (str/includes? (str (:documentation (first signatures))) "Says hello.")))
        (let [typed (str a "(str \"x\" ")]
          (notify! "textDocument/didChange" {:textDocument (assoc doc :version 3) :contentChanges [{:text typed}]})
          (is (some #(str/includes? (:label %) "str") (:signatures (request! "textDocument/signatureHelp" {:textDocument doc :position (end-of typed)})))
              "clojure.core"))))
    (testing "through an alias"
      (let [bdoc {:uri (uri root "src/app/b.clj")}
            typed (str b "(a/greet ")]
        (notify! "textDocument/didOpen" {:textDocument (assoc bdoc :languageId "clojure" :version 1 :text typed)})
        (is (= ["greet [who]"] (map :label (:signatures (request! "textDocument/signatureHelp" {:textDocument bdoc :position (end-of typed)})))))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest an-open-file-changed-on-disk
  ;; git checkout or a formatter rewrites an open file; the editor reloads
  ;; it without a save. Positions must follow what the index then has.
  (let [a "(ns app.a)\n(defn greet [who] who)\n(defn twice [x] (greet x))\n"
        a2 (str ";; one\n;; two\n" a)
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" a})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        doc {:uri (uri root "src/app/a.clj")}]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (notify! "textDocument/didOpen" {:textDocument (assoc doc :languageId "clojure" :version 1 :text a)})
    (wait-indexed! client)
    (spit (io/file root "src/app/a.clj") a2)
    (notify! "workspace/didChangeWatchedFiles" {:changes [{:uri (:uri doc) :type 2}]})
    (notify! "textDocument/didChange" {:textDocument (assoc doc :version 2) :contentChanges [{:text a2}]})
    (wait-indexed! client)
    (is (= [(pos-of a2 "greet [")]
           (map (comp :start :range) (request! "textDocument/definition" {:textDocument doc :position (pos-of a2 "greet x")}))))
    (testing "and saved again, unchanged"
      (notify! "textDocument/didSave" {:textDocument doc})
      (wait-indexed! client)
      (is (= [(pos-of a2 "greet [")]
             (map (comp :start :range) (request! "textDocument/definition" {:textDocument doc :position (pos-of a2 "greet x")})))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest symbol-kinds-follow-what-defined-them
  (let [a (str "(ns app.a (:require [clojure.spec.alpha :as s]))\n(defn f [] 1)\n(def v 1)\n(defmacro m [])\n"
               "(defprotocol P (pm [x]))\n(defrecord R [x])\n(defmulti mm identity)\n(s/def ::k int?)\n")
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" a})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (let [[ns-sym] (request! "textDocument/documentSymbol" {:textDocument {:uri (uri root "src/app/a.clj")}})
          kinds (into {} (map (juxt :name :kind)) (:children ns-sym))]
      (is (= 3 (:kind ns-sym)) "namespace")
      (is (= {"f" 12 "v" 13 "m" 12 "P" 11 "pm" 12 "R" 5 "mm" 11} (select-keys kinds ["f" "v" "m" "P" "pm" "R" "mm"])))
      (is (= 20 (kinds "k")) "a spec keyword"))
    (is (= 13 (:kind (first (filter #(= "v" (:name %)) (request! "workspace/symbol" {:query "v"}))))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest signature-parameters-and-arities
  (testing "parameters are found as whole tokens, past a type hint"
    (is (= [[9 10]] (map :label (:parameters (first (#'server/signature {:name "f" :arglists ["[^long n]"]} 0)))))))
  (let [a "(ns app.a)\n(defn bar ([a b] a) ([a b c] a))\n"
        root (project! {"deps.edn" "{:paths [\"src\"]}" "src/app/a.clj" a})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        doc {:uri (uri root "src/app/a.clj")}]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (let [typed (str a "(bar 1 2 3)")]
      (notify! "textDocument/didOpen" {:textDocument (assoc doc :languageId "clojure" :version 1 :text typed)})
      (testing "the arity is the one the whole call fits, wherever the cursor is"
        (let [{:keys [signatures activeSignature activeParameter]}
              (request! "textDocument/signatureHelp" {:textDocument doc :position (pos-of typed "2 3)")})]
          (is (= "bar [a b c]" (:label (nth signatures activeSignature))))
          (is (= 1 activeParameter)))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest the-editor-hears-of-a-classpath-that-failed
  (let [root (project! {"deps.edn" "{:paths [\"src\"" "src/app/a.clj" "(ns app.a) (defn f [] 1)"})
        {:keys [request! notify! notifications] :as client} (start! (str (tu/temp-dir)))]
    (request! "initialize" {:rootUri (convert/path->uri root) :capabilities {:window {:workDoneProgress true}}})
    (notify! "initialized" {})
    (wait-indexed! client)
    (is (loop [n 0]
          (or (some #(and (= "window/showMessage" (:method %))
                          (str/includes? (get-in % [:params :message]) "classpath"))
                    @notifications)
              (when (< n 100) (Thread/sleep 50) (recur (inc n))))))
    (is (seq (request! "workspace/symbol" {:query "f"})) "src/ is indexed meanwhile")
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest an-edit-in-a-local-root-dependency
  ;; the file is outside the project's root: the projects that have it
  ;; re-index it
  (let [lib (project! {"deps.edn" "{:paths [\"src\"]}" "src/lib/core.clj" "(ns lib.core) (defn old-fn [] 1)"})
        root (project! {"deps.edn" (pr-str {:paths ["src"] :deps {'my/lib {:local/root lib}}})
                        "src/app/a.clj" "(ns app.a (:require [lib.core]))"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        lib-file (io/file lib "src/lib/core.clj")]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (spit lib-file "(ns lib.core) (defn old-fn [] 1) (defn new-fn [] 2)")
    (notify! "textDocument/didSave" {:textDocument {:uri (convert/path->uri (.getCanonicalPath lib-file))}})
    (wait-indexed! client)
    (is (seq (filter #(= "new-fn" (:name %)) (request! "workspace/symbol" {:query "new-fn"}))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))

(deftest an-edited-hook-takes-effect
  (let [hook (fn [suffix] (str "(ns hooks.named (:require [clj-kondo.hooks-api :as api]))
                               (defn named [{:keys [node]}]
                                 (let [[_ n] (:children node)]
                                   {:node (api/list-node [(api/token-node 'def) (api/token-node (symbol (str (:value n) \"" suffix "\"))) (api/token-node 1)])}))"))
        root (project! {"deps.edn" "{:paths [\"src\"]}"
                        ".clj-kondo/config.edn" "{:hooks {:analyze-call {acme/named hooks.named/named}}}"
                        ".clj-kondo/hooks/named.clj" (hook "-one")
                        "src/acme.clj" "(ns acme) (defmacro named [n])"
                        "src/app/uses.clj" "(ns app.uses (:require [acme])) (acme/named x)"})
        {:keys [request! notify!] :as client} (start! (str (tu/temp-dir)))
        hook-file (io/file root ".clj-kondo/hooks/named.clj")]
    (request! "initialize" {:rootUri (convert/path->uri root)})
    (notify! "initialized" {})
    (wait-indexed! client)
    (spit hook-file (hook "-two"))
    (notify! "workspace/didChangeWatchedFiles" {:changes [{:uri (convert/path->uri (.getCanonicalPath hook-file)) :type 2}]})
    (wait-indexed! client)
    (is (seq (filter #(= "x-two" (:name %)) (request! "workspace/symbol" {:query "x-two"}))))
    (request! "shutdown" nil)
    (notify! "exit" nil)
    (deref (:server client) 10000 :timeout)))
