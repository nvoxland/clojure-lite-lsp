(ns csl.lsp.server
  "The LSP server (`csl lsp`): reading features only (DESIGN.md §8).

  Queries run on a read-only connection; editor events become work in the
  queue for the daemon, which this server makes sure is running. Positions
  in edited buffers are mapped onto the indexed version and back
  (csl.lsp.buffers)."
  (:refer-clojure :exclude [run!])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.client :as client]
   [csl.daemon :as daemon]
   [csl.db :as db]
   [csl.lsp.buffers :as buffers]
   [csl.lsp.convert :as convert]
   [csl.lsp.jsonrpc :as rpc]
   [csl.query :as q]
   [csl.queue :as queue]
   [csl.snapshot :as snapshot]
   [csl.version :as version])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(defn- log [& xs]
  (binding [*out* *err*] (apply println "csl:" xs)))

;;;; projects and work

(defn- project-of
  "The project a path belongs to: the one with the longest root that
  contains it."
  [{:keys [projects]} path]
  (->> @projects
       (filter #(or (= (:root %) path) (str/starts-with? path (str (:root %) File/separator))))
       (sort-by (comp - count :root))
       first))

(defn- enqueue!
  "Queue work, and make sure a daemon is there to do it (it exits when
  idle; checking costs a lock probe)."
  [{:keys [client-c opts]} p kind path priority]
  (queue/enqueue! @client-c p kind path priority)
  (client/ensure-daemon! opts))

(def ^:private build-files #{"deps.edn" "project.clj" "bb.edn" ".csl.edn" "config.edn"})

(defn- file-changed! [state path deleted?]
  (when-let [{:keys [p]} (project-of state path)]
    (cond
      (build-files (.getName (io/file path))) (enqueue! state p :sync "" 1)
      deleted? (enqueue! state p :delete path 1)
      :else (enqueue! state p :file path 1))))

;;;; positions and results

(defn- text-position
  "The request's document path and indexed [row col], or nil."
  [{:keys [buffers]} {:keys [textDocument position]}]
  (let [path (convert/uri->path (:uri textDocument))]
    (when-let [[row col] (buffers/->indexed buffers path (convert/->kondo position))]
      [path row col])))

(defn- in-buffer
  "A result location with its position mapped into the open buffer of its
  file, or nil when that position was edited away."
  [{:keys [buffers]} {:keys [path entry pos] :as loc}]
  (if (or entry (nil? pos) (not (every? some? pos)))
    loc
    (some->> (buffers/->buffer buffers path pos) (assoc loc :pos))))

(defn- lsp-locations [{:keys [opts] :as state} locs]
  (->> locs (keep #(in-buffer state %)) (mapv #(convert/location % opts))))

(defn- with-project
  "Call (f c p path row col) for a text-position request, or return `none`."
  [state params none f]
  (if-let [[path row col] (text-position state params)]
    (if-let [{:keys [p]} (project-of state path)]
      (f @(:reader state) p path row col)
      none)
    none))

;;;; requests

(defn- hover-markdown [{:keys [kind ns name doc arglists]}]
  (str/join "\n\n"
            (remove nil?
                    [(str "```clojure\n" (if (and ns (not= :ns-def kind)) (str ns "/" name) name) "\n```")
                     (when (seq arglists) (str "```clojure\n" (str/join "\n" arglists) "\n```"))
                     doc])))

(def ^:private symbol-kinds {:ns-def 3 :var-def 12 :keyword-def 20})

(defn- document-symbols [state path p]
  (let [syms (keep (fn [{:keys [pos form] :as s}]
                     (when-let [sel (buffers/->buffer (:buffers state) path pos)]
                       (assoc s :sel sel :full (or (and form (buffers/->buffer (:buffers state) path form)) sel))))
                   (q/document-symbols @(:reader state) p path))
        ->sym (fn [{:keys [kind name sel full]}]
                {:name name :kind (symbol-kinds kind 13)
                 :range (convert/range full) :selectionRange (convert/range sel)})
        [nss defs] ((juxt filter remove) #(= :ns-def (:kind %)) syms)]
    (if (seq nss)
      (into [(assoc (->sym (first nss)) :children (mapv ->sym defs))] (map ->sym (rest nss)))
      (mapv ->sym defs))))

(defn- call-item [{:keys [opts]} {:keys [ns name locations]}]
  (when-let [loc (first locations)]
    (let [{:keys [uri range]} (convert/location loc opts)]
      {:name (or name ns) :kind (if name 12 3) :detail ns :uri uri
       :range range :selectionRange range :data {:ns ns :name name}})))

(defn- ranges-in [state calls]
  (mapv :range (lsp-locations state calls)))

(defn- handle-request [{:keys [opts reader projects] :as state} {:keys [method params]}]
  (case method
    "textDocument/definition"
    (with-project state params [] #(lsp-locations state (q/definition %1 %2 %3 %4 %5)))

    "textDocument/declaration"
    (with-project state params [] #(lsp-locations state (q/definition %1 %2 %3 %4 %5)))

    "textDocument/implementation"
    (with-project state params [] #(lsp-locations state (q/implementations %1 %2 %3 %4 %5)))

    "textDocument/references"
    (with-project state params []
      #(lsp-locations state (q/references %1 %2 %3 %4 %5
                                          {:include-declaration? (get-in params [:context :includeDeclaration])})))

    "textDocument/hover"
    (with-project state params nil
      (fn [c p path row col]
        (when-let [hs (seq (q/hover c p path row col))]
          {:contents {:kind "markdown" :value (str/join "\n\n---\n\n" (map hover-markdown hs))}})))

    "textDocument/documentSymbol"
    (let [path (convert/uri->path (get-in params [:textDocument :uri]))]
      (if-let [{:keys [p]} (project-of state path)] (document-symbols state path p) []))

    "workspace/symbol"
    (vec (for [{:keys [p]} @projects
               {:keys [kind ns name location]} (q/workspace-symbols @reader p (:query params) {:limit 200})
               :when location]
           {:name name :kind (symbol-kinds kind 13) :containerName ns
            :location (convert/location location opts)}))

    "textDocument/prepareCallHierarchy"
    (with-project state params [] (fn [c p path row col]
                                    (vec (keep #(call-item state %) (q/call-hierarchy-items c p path row col)))))

    "callHierarchy/incomingCalls"
    (let [{:keys [ns name]} (get-in params [:item :data])
          path (convert/uri->path (get-in params [:item :uri]))]
      (if-let [{:keys [p]} (project-of state path)]
        (vec (keep (fn [{:keys [caller calls]}]
                     (when-let [item (call-item state caller)]
                       {:from item :fromRanges (ranges-in state calls)}))
                   (q/incoming-calls @reader p ns name)))
        []))

    "callHierarchy/outgoingCalls"
    (let [{:keys [ns name]} (get-in params [:item :data])
          path (convert/uri->path (get-in params [:item :uri]))]
      (if-let [{:keys [p]} (project-of state path)]
        (vec (keep (fn [{:keys [callee calls]}]
                     (when-let [item (call-item state callee)]
                       {:to item :fromRanges (ranges-in state calls)}))
                   (q/outgoing-calls @reader p ns name)))
        []))

    "csl/status"
    {:pending (reduce + (map #(queue/pending-count @(:client-c state) (:p %)) @projects))
     :projects (mapv :root @projects)}

    "shutdown" nil

    ::unknown))

;;;; lifecycle

(defn- roots [{:keys [workspaceFolders rootUri rootPath]}]
  (->> (cond (seq workspaceFolders) (map (comp convert/uri->path :uri) workspaceFolders)
             rootUri [(convert/uri->path rootUri)]
             rootPath [rootPath])
       (map #(.getCanonicalPath (io/file ^String %)))
       distinct))

(defn- initialize! [{:keys [opts projects client-c reader] :as state} params]
  (let [{:keys [db]} (daemon/paths (:home opts))]
    (client/ensure-daemon! opts)
    (reset! client-c (db/open-client db))
    (reset! reader (db/open-reader db))
    (swap! (:opts-atom state) merge (select-keys (:initializationOptions params) [:dependency-scheme])
           {:progress? (boolean (get-in params [:capabilities :window :workDoneProgress]))})
    (reset! projects (vec (for [root (roots params)]
                            {:root root :p (snapshot/ensure-project! @client-c root)})))
    (doseq [{:keys [p]} @projects] (enqueue! state p :sync "" 1))
    {:capabilities {:textDocumentSync {:openClose true :change 1 :save {:includeText false}}
                    :definitionProvider true
                    :declarationProvider true
                    :implementationProvider true
                    :referencesProvider true
                    :hoverProvider true
                    :documentSymbolProvider true
                    :workspaceSymbolProvider true
                    :callHierarchyProvider true}
     :serverInfo {:name "clojure-sqlite-lsp" :version (:version opts)}}))

(defn- watch-files! [{:keys [send!]}]
  (send! {:jsonrpc "2.0" :id "csl-watch" :method "client/registerCapability"
          :params {:registrations
                   [{:id "csl-watched-files" :method "workspace/didChangeWatchedFiles"
                     :registerOptions {:watchers [{:globPattern "**/*.{clj,cljs,cljc,cljd,edn,bb}"}
                                                  {:globPattern "**/project.clj"}]}}]}}))

(defn- pending-total [c projects]
  (reduce + (map #(queue/pending-count c (:p %)) projects)))

(defn- report-progress!
  "While the server runs: report indexing as LSP work-done progress, from
  the queue's pending counts. Uses its own connection (JDBC connections are
  not shared between threads)."
  [{:keys [send! projects running? opts]}]
  (future
    (try
      (with-open [c (db/open-reader (:db (daemon/paths (:home opts))))]
        (loop [n 0 token nil begun? false shown nil]
          (when @running?
            (Thread/sleep 300)
            (let [pending (pending-total c @projects)]
              (cond
                (and (pos? pending) (nil? token))
                (let [token (str "csl-indexing-" n)]
                  (send! {:jsonrpc "2.0" :id (str "csl-progress-" n) :method "window/workDoneProgress/create"
                          :params {:token token}})
                  (recur (inc n) token false nil))

                ;; begin a tick after asking for the token, so the client has it
                (and token (not begun?))
                (do (send! {:jsonrpc "2.0" :method "$/progress"
                            :params {:token token :value {:kind "begin" :title "Indexing" :cancellable false
                                                          :message (str pending " pending")}}})
                    (recur n token true pending))

                (and token (zero? pending))
                (do (send! {:jsonrpc "2.0" :method "$/progress"
                            :params {:token token :value {:kind "end" :message "Indexed"}}})
                    (recur n nil false nil))

                (and token (not= pending shown))
                (do (send! {:jsonrpc "2.0" :method "$/progress"
                            :params {:token token :value {:kind "report" :message (str pending " pending")}}})
                    (recur n token true pending))

                :else (recur n token begun? shown))))))
      (catch Exception e (log "progress reporting stopped:" (ex-message e))))))

(defn- handle-notification [{:keys [buffers] :as state} {:keys [method params]}]
  (let [doc-path #(convert/uri->path (get-in params [:textDocument :uri]))]
    (case method
      "initialized" (do (watch-files! state)
                        (when (:progress? (:opts state)) (report-progress! state)))
      "textDocument/didOpen" (let [path (doc-path)]
                               (buffers/open! buffers path (get-in params [:textDocument :text]))
                               (when-let [{:keys [p]} (project-of state path)] (enqueue! state p :file path 0)))
      "textDocument/didChange" (buffers/change! buffers (doc-path) (:text (last (:contentChanges params))))
      "textDocument/didSave" (let [path (doc-path)]
                               (buffers/saved! buffers path)
                               (when-let [{:keys [p]} (project-of state path)] (enqueue! state p :file path 0)))
      "textDocument/didClose" (buffers/close! buffers (doc-path))
      "workspace/didChangeWatchedFiles" (doseq [{:keys [uri type]} (:changes params)
                                                :when (str/starts-with? uri "file:")]
                                          (file-changed! state (convert/uri->path uri) (= 3 type)))
      nil)))

(defn run!
  "Serve LSP on `in`/`out` until `exit`. Returns the exit code."
  [{:keys [in out home version spawn!] :or {version version/version}}]
  (let [opts-atom (atom {:home home :version version :spawn! spawn!})
        state {:opts-atom opts-atom
               :projects (atom [])
               :client-c (atom nil)
               :reader (atom nil)
               :buffers (buffers/store)
               :running? (atom true)
               :send! #(rpc/write-message! out %)}
        state-now #(assoc state :opts @opts-atom)
        reply! (fn [id m] ((:send! state) (merge {:jsonrpc "2.0" :id id} m)))]
    (try
      (loop [shutdown? false]
        (let [{:keys [id method] :as msg} (rpc/read-message in)]
          (cond
            (nil? msg) 1
            (= "exit" method) (if shutdown? 0 1)
            ;; a response to one of our requests
            (and id (nil? method)) (recur shutdown?)
            (nil? id) (do (try (handle-notification (state-now) msg)
                               (catch Exception e (log method "failed:" (ex-message e))))
                          (recur shutdown?))
            :else
            (do (try
                  (let [result (if (= "initialize" method)
                                 (initialize! (state-now) (:params msg))
                                 (handle-request (state-now) msg))]
                    (if (= ::unknown result)
                      (reply! id {:error {:code -32601 :message (str "Unsupported: " method)}})
                      (reply! id {:result result})))
                  (catch Exception e
                    (log method "failed:" (ex-message e))
                    (reply! id {:error {:code -32603 :message (str (ex-message e))}})))
                (recur (or shutdown? (= "shutdown" method)))))))
      (finally
        (reset! (:running? state) false)
        (doseq [a [(:client-c state) (:reader state)]]
          (some-> ^java.sql.Connection @a .close))))))
