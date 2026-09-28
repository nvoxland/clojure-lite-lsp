(ns clojure-lite-lsp.lsp.server
  "The LSP server (`clojure-lite-lsp lsp`): reading features only.

  Queries run on a read-only connection; editor events become work in the
  queue for the daemon, which this server makes sure is running. Positions
  in edited buffers are mapped onto the indexed version and back
  (clojure-lite-lsp.lsp.buffers)."
  (:require
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.java :as java]
   [clojure-lite-lsp.lock :as lock]
   [clojure-lite-lsp.log :as log]
   [clojure-lite-lsp.lsp.buffers :as buffers]
   [clojure-lite-lsp.lsp.convert :as convert]
   [clojure-lite-lsp.lsp.forms :as forms]
   [clojure-lite-lsp.lsp.jsonrpc :as rpc]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.sources :as sources]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.sql Connection]
   [java.util Arrays]))

(set! *warn-on-reflection* true)

(defn- client-path
  "The canonical path of a file: URI from the editor (nil for other URIs).
  Editors send paths as the project was opened, possibly through symlinks;
  the index has canonical ones."
  [uri]
  (some-> (convert/uri->path uri) (File.) (.getCanonicalPath)))

(defn- notify!
  "Send the client a notification."
  [{:keys [send!]} method params]
  (send! {:jsonrpc "2.0" :method method :params params}))

(defn- request!
  "Send the client a request (its response is ignored)."
  [{:keys [send!]} id method params]
  (send! {:jsonrpc "2.0" :id id :method method :params params}))

;;;; projects and work

(defn- project-of
  "The project a path belongs to: the one with the longest root that
  contains it. An extracted library file belongs to a project with its jar
  on the classpath."
  [{:keys [projects reader opts]} path]
  (if-let [{:keys [jar-hash-hex]} (sources/source-of (:home opts) path)]
    (let [ps (set (map first (db/query @reader "SELECT pj.project_id FROM project_jar pj JOIN jar j ON j.id = pj.jar_id
                                                WHERE j.jar_hash = ?" (digest/unhex jar-hash-hex))))]
      (some #(when (ps (:p %)) %) @projects))
    (when path
      (->> @projects
           (filter #(or (= (:root %) path) (str/starts-with? path (str (:root %) File/separator))))
           (sort-by (comp - count :root))
           first))))

(defn- dep-file? [{:keys [opts]} path] (some? (sources/source-of (:home opts) path)))

(defn- enqueue!
  "Queue work, and make sure a daemon is there to do it (it exits when
  idle): a lock probe."
  [{:keys [client-c opts]} p kind path priority]
  (queue/enqueue! @client-c p kind path priority)
  (client/ensure-daemon-alive! opts))

(def ^:private build-files (set classpath/build-files))

(defn- build-file?
  "Does a change to `path` change project `root`'s classpath or clj-kondo
  config (hooks included)?"
  [root path]
  (let [f (io/file path)]
    (or (and (build-files (.getName f)) (= root (.getParent f)))
        (str/starts-with? path (str (io/file root ".clj-kondo") File/separator)))))

(defn- projects-with
  "The projects `path` is part of: `own` (its project by root, or nil), and
  any other that has it (a :local/root dependency's file)."
  [{:keys [reader]} own path]
  (distinct (concat (some-> own :p vector)
                    (map first (db/query @reader "SELECT DISTINCT project_id FROM project_file WHERE path = ?" path)))))

(defn- file-changed! [{:keys [buffers] :as state} path deleted?]
  ;; an open document's file rewritten outside the editor
  (when (and (not deleted?) (contains? @buffers path) (.isFile (io/file path)))
    (buffers/changed-on-disk! buffers path (slurp path)))
  (let [{:keys [p root] :as own} (project-of state path)]
    (when (and own (build-file? root path))
      (enqueue! state p :sync "" 1))
    (doseq [p (projects-with state own path)]
      (enqueue! state p (if deleted? :delete :file) path 1))))

;;;; positions and results

(defn- text-position
  "The request's document path and indexed [row col], or nil."
  [{:keys [buffers]} {:keys [textDocument position]}]
  (when-let [path (client-path (:uri textDocument))]
    (when-let [[row col] (buffers/->indexed buffers path (convert/->kondo position))]
      [path row col])))

(defn- in-buffer
  "A result location with its position mapped into the open buffer of its
  file, or nil when that position was edited away."
  [{:keys [buffers]} {:keys [path entry pos] :as loc}]
  (if (or entry (nil? pos) (not-every? some? pos))
    loc
    (some->> (buffers/->buffer buffers path pos) (assoc loc :pos))))

(defn- as-file
  "A location in a jar as its extracted file (`extract` is
  sources/extract! or extract-soon!), unless the client asked for jar: or
  zipfile: URIs. nil when it can't be extracted."
  ([state loc] (as-file state loc sources/extract!))
  ([{:keys [opts]} {:keys [entry] :as loc} extract]
   (if (and entry (= "file" (:dependency-scheme opts "file")))
     (when-let [path (extract (:home opts) loc)]
       (assoc (dissoc loc :entry :jar-hash) :path path))
     loc)))

(defn- lsp-location
  "Location `loc` as an LSP Location: extracted from its jar (`extract`,
  as `as-file`), and mapped into its open buffer. nil when either can't be
  done."
  ([state loc] (lsp-location state loc sources/extract!))
  ([{:keys [opts] :as state} loc extract]
   (when-let [loc (some->> (as-file state loc extract) (in-buffer state))]
     (convert/location loc opts))))

(defn- resolve-java
  "A {:java-class} result as the location of its source, or nil."
  [{:keys [reader opts]} p {:keys [java-class] :as loc}]
  (if java-class
    (java/class-location @reader (:home opts) p java-class)
    loc))

(defn- lsp-locations [state p locs]
  (into [] (keep #(some->> (resolve-java state p %) (lsp-location state))) locs))

(defn- with-project
  "Call (f c p path row col) for a text-position request, `c` the reader
  connection, or return `none`."
  [state params none f]
  (if-let [[path row col] (text-position state params)]
    (if-let [{:keys [p]} (project-of state path)]
      (f @(:reader state) p path row col)
      none)
    none))

(defn- locations-at
  "The LSP locations of what (query c p path row col & args) finds for a
  text-position request."
  [state params query & args]
  (with-project state params []
    (fn [c p path row col] (lsp-locations state p (apply query c p path row col args)))))

;;;; requests

(defn- highlights [{:keys [buffers]} c p path row col]
  (into []
        (keep (fn [{:keys [pos write?]}]
                (when-let [bp (buffers/->buffer buffers path pos)]
                  ;; DocumentHighlightKind: 2 read, 3 write
                  {:range (convert/range bp) :kind (if write? 3 2)})))
        (q/highlights c p path row col)))

(defn- token-index
  "Where `token` occurs in `s` from `from` as a whole token (not inside a
  type hint like the n in ^long)."
  [^String s ^String token from]
  (let [delimiter? #(or (nil? %) (Character/isWhitespace (char %)) (#{\[ \] \( \) \{ \}} %))]
    (loop [from from]
      (when-let [i (str/index-of s token from)]
        (if (and (delimiter? (when (pos? i) (.charAt s (dec i))))
                 (delimiter? (when (< (+ i (count token)) (count s)) (.charAt s (+ i (count token))))))
          i
          (recur (inc i)))))))

(defn- signature
  "The signatures of `info`'s arglists for a call at argument `arg` with
  `total` arguments."
  [{:keys [name arglists doc]} arg total]
  (let [total (max total (inc arg))]
    (for [arglist arglists
          :let [label (str name " " arglist)
                {:keys [params variadic]} (forms/arglist-params arglist)
                offsets (loop [[t & more] params from (inc (count name)) out []]
                          (if-let [i (when t (token-index label t from))]
                            (recur more (+ i (count t)) (conj out [i (+ i (count t))]))
                            out))]]
      {:label label
       :documentation (when doc {:kind "markdown" :value doc})
       :parameters (mapv (fn [o] {:label o}) offsets)
       ;; the whole call's arguments choose the arity
       :fits? (if variadic (>= total variadic) (= total (count params)))
       :close? (or (some? variadic) (<= total (count params)))
       ;; past the last parameter: the last one (LSP reads an index out
       ;; of range as the first)
       :active (if (and variadic (>= arg variadic))
                 variadic
                 (min arg (max 0 (dec (count params)))))})))

(defn- index-where [pred coll] (first (keep-indexed #(when (pred %2) %1) coll)))

(defn- call-infos
  "What the call around the cursor calls, as hover infos, with the
  argument the cursor is at and the call's argument count, or nil."
  [{:keys [buffers reader]} p path text position]
  (when-let [{[start end] :head :keys [arg] total :count} (forms/call-at text (buffers/offset text position))]
    (let [c @reader
          ;; the head where the index knows it, else resolved by name
          [row col] (buffers/->indexed buffers path (convert/->kondo (buffers/position text start)))]
      {:infos (or (seq (when row (q/hover c p path row col)))
                  (q/hover-of-elements c p (q/resolve-symbol c p path (subs text start end))))
       :arg arg
       :total total})))

(defn- signature-help [{:keys [buffers] :as state} {:keys [textDocument position]}]
  (when-let [path (client-path (:uri textDocument))]
    (when-let [{:keys [p]} (project-of state path)]
      (when-let [text (buffers/text buffers path)]
        (let [{:keys [infos arg total]} (call-infos state p path text position)
              sigs (into [] (comp (filter (comp seq :arglists)) (mapcat #(signature % arg total))) infos)]
          (when (seq sigs)
            (let [active (or (index-where :fits? sigs) (index-where :close? sigs) 0)]
              {:signatures (mapv #(dissoc % :fits? :close? :active) sigs)
               :activeSignature active
               :activeParameter (:active (nth sigs active))})))))))

(defn- hover-markdown [{:keys [kind ns name doc arglists]}]
  (str/join "\n\n"
            (remove nil?
                    [(str "```clojure\n" (if (and ns (not= :ns-def kind)) (str ns "/" name) name) "\n```")
                     (when (seq arglists) (str "```clojure\n" (str/join "\n" arglists) "\n```"))
                     doc])))

(defn- symbol-kind
  "The LSP SymbolKind of a definition, from what defined it: Namespace 3,
  Class 5, Interface 11, Function 12, Variable 13, Key 20. A protocol's or
  record's own fns (methods, constructors) have arities; it doesn't."
  [{:keys [kind defined-by extra]}]
  (let [definer (some-> defined-by (str/replace #"^.*/" ""))
        callable? (or (:fixed-arities extra) (:varargs-min-arity extra) (seq (:arglist-strs extra)))]
    (case kind
      :ns-def 3
      :keyword-def 20
      (case definer
        ("defprotocol" "definterface") (if callable? 12 11)
        ("defrecord" "deftype") (if callable? 12 5)
        "defmulti" 11
        ("def" "defonce") 13
        12))))

(defn- document-symbols [{:keys [buffers reader]} p path]
  (let [syms (keep (fn [{:keys [pos form] :as s}]
                     (when-let [sel (buffers/->buffer buffers path pos)]
                       (assoc s :sel sel :full (or (some->> form (buffers/->buffer buffers path)) sel))))
                   (q/document-symbols @reader p path))
        ->sym (fn [{:keys [name sel full] :as s}]
                {:name name :kind (symbol-kind s)
                 :range (convert/range full) :selectionRange (convert/range sel)})
        [nss defs] ((juxt filter remove) #(= :ns-def (:kind %)) syms)]
    (if (seq nss)
      (into [(assoc (->sym (first nss)) :children (mapv ->sym defs))] (map ->sym (rest nss)))
      (mapv ->sym defs))))

(defn- call-item
  "A call hierarchy item for a function (or, :name nil, a namespace's top
  level)."
  [state {:keys [ns name locations]}]
  (when-let [{:keys [uri range]} (some #(lsp-location state %) locations)]
    {:name (or name ns) :kind (if name 12 3) :detail ns :uri uri
     :range range :selectionRange range :data {:ns ns :name name}}))

(defn- calls
  "The call hierarchy calls of the request's item: `query` is
  q/incoming-calls or q/outgoing-calls, `other` the key of each call's
  other end in its results (:caller, :callee), `as` the LSP key for it
  (:from, :to)."
  [{:keys [reader] :as state} params query other as]
  (let [{:keys [ns name]} (get-in params [:item :data])]
    (if-let [{:keys [p]} (project-of state (client-path (get-in params [:item :uri])))]
      (into []
            (keep (fn [{call-sites :calls who other}]
                    (when-let [item (call-item state who)]
                      {as item :fromRanges (mapv :range (lsp-locations state p call-sites))})))
            (query @reader p ns name))
      [])))

(defn- catch-up-buffers!
  "Documents whose file's text the index now has: that text is the base.
  By content: the index's hash of the file is the saved text's, and
  nothing for the file is still queued."
  [{:keys [buffers reader]}]
  (doseq [[path h] (buffers/awaiting buffers)
          :let [indexed (db/query-value @reader "SELECT content_hash FROM fingerprint WHERE path = ?" path)]
          :when (and indexed (Arrays/equals ^bytes indexed ^bytes h)
                     (nil? (db/query-value @reader "SELECT 1 FROM pending WHERE kind = 'file' AND path = ?" path)))]
    (buffers/indexed! buffers path)))

(def ^:private index-wait-ms
  "How long a request waits for the index (waitForIndex) at most."
  120000)

(defn- pending-total [c projects]
  (reduce + (map #(queue/pending-count c (:p %)) projects)))

(defn- await-index!
  "Wait until the projects have nothing queued: for clients that asked to
  (initializationOptions {\"waitForIndex\": true}), agents that ask once and
  trust the answer. Editors are served from what's indexed so far."
  [{:keys [reader projects]}]
  (let [deadline (+ (System/currentTimeMillis) index-wait-ms)]
    (loop []
      (when (and (pos? (pending-total @reader @projects))
                 (< (System/currentTimeMillis) deadline))
        (Thread/sleep 100)
        (recur)))))

(defn- workspace-symbols
  "Symbol search over every project, library hits extracted in the
  background: each would be a jar opened and a file written, on every
  keystroke."
  [{:keys [projects reader] :as state} query]
  (vec (for [{:keys [p]} @projects
             {:keys [ns name location] :as sym} (q/workspace-symbols @reader p query {:limit 200})
             :let [loc (lsp-location state location sources/extract-soon!)]
             :when loc]
         {:name name :kind (symbol-kind sym) :containerName ns :location loc})))

(defn- handle-request [{:keys [opts reader projects] :as state} {:keys [method params]}]
  (when (:waitForIndex opts) (await-index! state))
  (catch-up-buffers! state)
  (case method
    "textDocument/definition" (locations-at state params q/definition)
    "textDocument/declaration" (locations-at state params q/declaration)
    "textDocument/implementation" (locations-at state params q/implementations)
    "textDocument/references" (locations-at state params q/references
                                            {:include-declaration? (get-in params [:context :includeDeclaration])})
    "textDocument/documentHighlight" (with-project state params [] (partial highlights state))
    "textDocument/signatureHelp" (signature-help state params)

    "textDocument/hover"
    (with-project state params nil
      (fn [c p path row col]
        (when-let [hs (seq (q/hover c p path row col))]
          {:contents {:kind "markdown" :value (str/join "\n\n---\n\n" (map hover-markdown hs))}})))

    "textDocument/documentSymbol"
    (let [path (client-path (get-in params [:textDocument :uri]))]
      (if-let [{:keys [p]} (project-of state path)] (document-symbols state p path) []))

    "workspace/symbol" (workspace-symbols state (:query params))

    "textDocument/prepareCallHierarchy"
    (with-project state params [] (fn [c p path row col]
                                    (into [] (keep #(call-item state %)) (q/call-hierarchy-items c p path row col))))

    "callHierarchy/incomingCalls" (calls state params q/incoming-calls :caller :from)
    "callHierarchy/outgoingCalls" (calls state params q/outgoing-calls :callee :to)

    "clojure-lite-lsp/status" {:pending (pending-total @reader @projects) :projects (mapv :root @projects)}

    "shutdown" nil

    ::unknown))

;;;; lifecycle

(defn- roots [{:keys [workspaceFolders rootUri rootPath]}]
  (->> (cond (seq workspaceFolders) (map (comp convert/uri->path :uri) workspaceFolders)
             rootUri [(convert/uri->path rootUri)]
             rootPath [rootPath])
       ;; a root that isn't a file: URI has no path
       (keep #(some-> % io/file .getCanonicalPath))
       distinct))

(defn- initialize! [{:keys [opts projects client-c reader] :as state} params]
  (let [{:keys [db]} (home/paths (:home opts))]
    (client/ensure-daemon! opts)
    (reset! client-c (db/open-client db))
    (reset! reader (db/open-reader db))
    (swap! (:opts-atom state) merge (select-keys (:initializationOptions params) [:dependency-scheme :waitForIndex])
           {:progress? (boolean (get-in params [:capabilities :window :workDoneProgress]))})
    (reset! projects (vec (for [root (roots params)]
                            {:root root :p (snapshot/ensure-project! @client-c root)})))
    (doseq [{:keys [p]} @projects] (enqueue! state p :sync "" 1))
    {:capabilities {:textDocumentSync {:openClose true :change 2 :save {:includeText false}}
                    :definitionProvider true
                    :declarationProvider true
                    :implementationProvider true
                    :referencesProvider true
                    :hoverProvider true
                    :documentSymbolProvider true
                    :workspaceSymbolProvider true
                    :callHierarchyProvider true
                    :documentHighlightProvider true
                    :signatureHelpProvider {:triggerCharacters ["(" " "]}}
     :serverInfo {:name "clojure-lite-lsp" :version (:version opts)}}))

(defn- watch-files! [state]
  (request! state "clojure-lite-lsp-watch" "client/registerCapability"
            {:registrations
             [{:id "clojure-lite-lsp-watched-files" :method "workspace/didChangeWatchedFiles"
               :registerOptions {:watchers [{:globPattern "**/*.{clj,cljs,cljc,cljd,edn,bb}"}
                                            {:globPattern "**/project.clj"}]}}]}))

(defn- warn-of-classpath-errors!
  "While the server runs: tell the user, once per error, when a project's
  classpath couldn't be computed (a broken build file, the build tool not
  on the editor's PATH): src/ and test/ are indexed meanwhile."
  [{:keys [projects running? opts] :as state}]
  (future
    (try
      (with-open [c (db/open-reader (:db (home/paths (:home opts))))]
        (loop [told {}]
          (when @running?
            (Thread/sleep 1000)
            (recur (reduce (fn [told {:keys [p root]}]
                             (let [e (classpath/error c p)]
                               (when (and e (not= e (told p)))
                                 ;; MessageType 2: warning
                                 (notify! state "window/showMessage"
                                          {:type 2
                                           :message (str "clojure-lite-lsp couldn't compute the classpath of " root
                                                         " (indexing its src/ and test/ meanwhile): " e)}))
                               (assoc told p e)))
                           told @projects)))))
      (catch Exception e (log/warn "classpath warnings stopped:" (ex-message e))))))

(defn- report-progress!
  "While the server runs: report indexing as LSP work-done progress, from
  the queue's pending counts. Uses its own connection (JDBC connections are
  not shared between threads)."
  [{:keys [projects running? opts] :as state}]
  (future
    (try
      (with-open [c (db/open-reader (:db (home/paths (:home opts))))]
        (loop [n 0 token nil begun? false shown nil]
          (when @running?
            (Thread/sleep 300)
            (let [pending (pending-total c @projects)]
              (cond
                (and (pos? pending) (nil? token))
                (let [token (str "clojure-lite-lsp-indexing-" n)]
                  (request! state (str "clojure-lite-lsp-progress-" n) "window/workDoneProgress/create" {:token token})
                  (recur (inc n) token false nil))

                ;; begin a tick after asking for the token, so the client has it
                (and token (not begun?))
                (do (notify! state "$/progress" {:token token :value {:kind "begin" :title "Indexing" :cancellable false
                                                                      :message (str pending " pending")}})
                    (recur n token true pending))

                (and token (zero? pending))
                (do (notify! state "$/progress" {:token token :value {:kind "end" :message "Indexed"}})
                    (recur n nil false nil))

                (and token (not= pending shown))
                (do (notify! state "$/progress" {:token token :value {:kind "report" :message (str pending " pending")}})
                    (recur n token true pending))

                :else (recur n token begun? shown))))))
      (catch Exception e (log/warn "progress reporting stopped:" (ex-message e))))))

(defn- handle-notification [{:keys [buffers] :as state} {:keys [method params]}]
  (let [path (client-path (get-in params [:textDocument :uri]))]
    ;; documents that aren't files (jar: entries) aren't tracked yet
    (when-not (and (str/starts-with? method "textDocument/") (nil? path))
      (case method
        "initialized" (do (watch-files! state)
                          (warn-of-classpath-errors! state)
                          (when (:progress? (:opts state)) (report-progress! state)))
        "textDocument/didOpen" (do (buffers/open! buffers path (get-in params [:textDocument :text]))
                                   (when-let [{:keys [p]} (project-of state path)]
                                     (enqueue! state p (if (dep-file? state path) :dep-file :file) path 0)))
        "textDocument/didChange" (buffers/change! buffers path (:contentChanges params))
        "textDocument/didSave" (do (buffers/saved! buffers path)
                                   (let [own (project-of state path)]
                                     (if (dep-file? state path)
                                       (when own (enqueue! state (:p own) :dep-file path 0))
                                       (doseq [p (projects-with state own path)]
                                         (enqueue! state p :file path 0)))))
        "textDocument/didClose" (buffers/close! buffers path)
        "workspace/didChangeWatchedFiles" (doseq [{:keys [uri type]} (:changes params)
                                                  :when (str/starts-with? uri "file:")]
                                            ;; one failure mustn't lose the rest (a checkout
                                            ;; reports many files at once); FileChangeType 3: deleted
                                            (try (file-changed! state (client-path uri) (= 3 type))
                                                 (catch Exception e (log/warn "watched file" uri "failed:" (ex-message e)))))
        nil))))

(defn serve!
  "Serve LSP on `in`/`out` until `exit`. Returns the exit code."
  [{:keys [in out home version spawn!] :or {version version/version}}]
  (let [;; set once more by initialize (the client's options); each message
        ;; is handled with the options as they are then (`state-now`)
        opts-atom (atom {:home home :version version :spawn! spawn!})
        state {:opts-atom opts-atom
               :projects (atom [])
               :client-c (atom nil)
               :reader (atom nil)
               :buffers (buffers/store)
               :running? (atom true)
               :send! #(rpc/write-message! out %)}
        state-now #(assoc state :opts @opts-atom)
        reply! (fn [id m] ((:send! state) (merge {:jsonrpc "2.0" :id id} m)))
        ;; held while this editor runs: its index is in use (home/paths)
        in-use (try (lock/try-share (:clients-lock (home/paths home))) (catch Exception _ nil))]
    (try
      (loop [shutdown? false]
        (let [{:keys [id method] :as msg} (rpc/read-message in)]
          (cond
            (nil? msg) 1
            (::rpc/parse-error msg) (do (reply! nil {:error {:code -32700 :message (::rpc/parse-error msg)}})
                                        (recur shutdown?))
            (= "exit" method) (if shutdown? 0 1)
            ;; a response to one of our requests
            (and id (nil? method)) (recur shutdown?)
            (nil? id) (do (try (handle-notification (state-now) msg)
                               (catch Exception e (log/warn method "failed:" (ex-message e))))
                          (recur shutdown?))
            (and shutdown? id)
            (do (reply! id {:error {:code -32600 :message "The server is shutting down"}})
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
                    (let [message (or (ex-message e) (str e))]
                      (log/warn method "failed:" message)
                      (reply! id {:error {:code -32603 :message message}}))))
                (recur (or shutdown? (= "shutdown" method)))))))
      (finally
        (some-> in-use lock/release!)
        (reset! (:running? state) false)
        (doseq [a [(:client-c state) (:reader state)]]
          (some-> ^Connection @a .close))))))
