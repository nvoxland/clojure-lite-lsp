(ns clojure-lite-lsp.commands
  "The query commands: what `clojure-lite-lsp query` and the MCP server
  (clojure-lite-lsp.mcp) offer, for agents as much as for people. One table
  drives the CLI's help, its dispatch and the MCP tools, so they always
  agree.

  A command's target is a symbol (`app.core/foo`, or `app.core` for a
  namespace) or a position (`path:line:col`, 1-based, as editors and
  compilers print them). Results are data ({:results [...]}) or grep-like
  text: `path:line:col: what's there`, paths relative to the project root."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.java :as java]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.sources :as sources])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

;;;; targets

(defn- file-of
  "`path` as a canonical file path: absolute, or relative to `cwd` (else
  the project root)."
  [{:keys [cwd root]} path]
  (let [f (io/file path)]
    (.getCanonicalPath
     (cond
       (.isAbsolute f) f
       ;; as typed from where the caller is, else as results show it
       (and cwd (.exists (io/file cwd path))) (io/file cwd path)
       :else (io/file root path)))))

(defn- target
  "{:position [path row col]} or {:symbol s}."
  [ctx s]
  (if-let [[_ path row col] (re-matches #"(.+):(\d+):(\d+)" s)]
    {:position [(file-of ctx path) (parse-long row) (parse-long col)]}
    {:symbol s}))

(defn- elements
  "What target string `s` means: elements, as at a position."
  [{:keys [c p] :as ctx} s]
  (let [{:keys [position symbol]} (target ctx s)]
    (if position
      (let [[path row col] position] (q/elements-at c p path row col))
      (q/symbol-elements c p symbol))))

(defn- vars
  "The [ns name] vars target `s` means: a definition or a use names one."
  [ctx s]
  (->> (elements ctx s)
       (filter (comp #{:var-def :var-usage :symbol-usage} :kind))
       (map (juxt :ns :name))
       (filter #(every? some? %))
       distinct))

;;;; locations

(defn- row
  "A location from the query layer as {:path :line :column :end-line
  :end-column}, or nil when it has no file: a jar entry is extracted, a
  Java class found in its sources."
  [{:keys [c p home]} {:keys [entry java-class pos] :as loc}]
  (let [{:keys [path pos]} (cond
                             java-class (java/class-location c home p java-class)
                             entry (when-let [f (sources/extract! home loc)] {:path f :pos pos})
                             :else loc)
        [line col end-line end-col] pos]
    (when (and path line)
      {:path path :line line :column col :end-line end-line :end-column end-col})))

(defn- rows [ctx locs] (vec (distinct (keep #(row ctx %) locs))))

(defn- file-lines [path]
  (try (with-open [r (io/reader (io/file path))] (vec (line-seq r)))
       (catch Exception _ [])))

(defn- source-line
  "Line `n` (1-based) of the file at `path`, trimmed, or nil. Files are
  read once per command (`:lines-of` in ctx)."
  [{:keys [lines-of] :or {lines-of file-lines}} path n]
  (some-> (get (lines-of path) (dec n)) str/trim))

(defn- shown-path
  "`path` relative to the project root when it's under it."
  [{:keys [root]} ^String path]
  (let [prefix (str root File/separator)]
    (if (str/starts-with? path prefix) (subs path (count prefix)) path)))

(defn- at [ctx {:keys [path line column]}]
  (str (shown-path ctx path) ":" line ":" column))

(defn- symbol-name [ns name] (if (and ns name) (str ns "/" name) (or name ns)))

;;;; the commands

(defn- locations-command [f]
  (fn [ctx arg] {:results (rows ctx (f ctx arg))}))

(defn- location-text [ctx {:keys [path line] :as r}]
  (str (at ctx r) ": " (source-line ctx path line)))

(def commands
  [{:name "definition"
    :usage "<symbol | file:line:col>"
    :doc "Where a var or namespace is defined."
    :run (locations-command (fn [{:keys [c p] :as ctx} arg] (q/definition-of-elements c p (elements ctx arg))))
    :line location-text}

   {:name "references"
    :usage "<symbol | file:line:col>"
    :doc "Every use of a var, namespace or keyword."
    :run (locations-command (fn [{:keys [c p] :as ctx} arg] (q/references-of-elements c p (elements ctx arg) {})))
    :line location-text}

   {:name "implementations"
    :usage "<symbol | file:line:col>"
    :doc "Implementations of a protocol or protocol method, and a multimethod's methods."
    :run (locations-command (fn [{:keys [c p] :as ctx} arg] (q/implementations-of-elements c p (elements ctx arg))))
    :line location-text}

   {:name "doc"
    :usage "<symbol | file:line:col>"
    :doc "A var's or namespace's arglists and docstring."
    :run (fn [{:keys [c p] :as ctx} arg]
           {:results (vec (for [{:keys [ns name kind doc arglists location]} (q/hover-of-elements c p (elements ctx arg))]
                            (merge {:symbol (if (= :ns-def kind) name (symbol-name ns name))
                                    :arglists (vec arglists) :doc doc}
                                   (some->> location (row ctx)))))})
    :line (fn [ctx {:keys [symbol arglists doc path] :as r}]
            (str/join "\n" (remove nil? [symbol
                                         (when (seq arglists) (str/join " " arglists))
                                         doc
                                         (when path (at ctx r))])))}

   {:name "callers"
    :usage "<symbol | file:line:col>"
    :doc "Where a function is called, and from which function."
    :run (fn [{:keys [c p] :as ctx} arg]
           {:results (vec (for [[ns name] (vars ctx arg)
                                {:keys [caller calls]} (q/incoming-calls c p ns name)
                                r (rows ctx calls)]
                            (assoc r :caller (symbol-name (:ns caller) (:name caller)))))})
    :line (fn [ctx r] (str (at ctx r) ": " (:caller r) " calls it"))}

   {:name "callees"
    :usage "<symbol | file:line:col>"
    :doc "What a function calls, and where."
    :run (fn [{:keys [c p] :as ctx} arg]
           {:results (vec (for [[ns name] (vars ctx arg)
                                {:keys [callee calls]} (q/outgoing-calls c p ns name)
                                r (rows ctx calls)]
                            (assoc r :callee (symbol-name (:ns callee) (:name callee)))))})
    :line (fn [ctx r] (str (at ctx r) ": calls " (:callee r)))}

   {:name "symbols"
    :usage "<text>"
    :doc "Definitions whose name contains the text, exact names first, then the project's own."
    :run (fn [{:keys [c p] :as ctx} text]
           {:results (vec (for [{:keys [kind ns name location]} (q/workspace-symbols c p text {:limit 50})
                                :let [r (row ctx location)]
                                :when r]
                            (assoc r :symbol (if (= :ns-def kind) name (symbol-name ns name)) :kind (clojure.core/name kind))))})
    :line (fn [ctx r] (str (at ctx r) ": " (:symbol r)))}

   {:name "outline"
    :usage "<file>"
    :doc "The namespaces and vars a file defines, in order."
    :run (fn [{:keys [c p] :as ctx} file]
           (let [path (file-of ctx file)]
             {:results (vec (for [{:keys [kind ns name pos]} (q/document-symbols c p path)
                                  :let [[line col end-line end-col] pos]]
                              {:path path :line line :column col :end-line end-line :end-column end-col
                               :kind (clojure.core/name kind)
                               :symbol (if (= :ns-def kind) name (symbol-name ns name))}))}))
    :line (fn [ctx {:keys [kind symbol] :as r}]
            (str (at ctx r) ": " (if (= "ns-def" kind) (str "ns " symbol) symbol)))}])

(def ^:private by-name (into {} (map (juxt :name identity)) commands))

(defn run
  "Run query command `args` ([name arg]) in `ctx` ({:c :p :root :home
  :cwd}): {:results [...] :total n}, at most `limit` results."
  [ctx [cmd arg] & [{:keys [limit]}]]
  (let [{:keys [run]} (or (by-name cmd)
                          (throw (ex-info (str "Unknown query command: " cmd ". Commands: "
                                               (str/join ", " (map :name commands)))
                                          {:command cmd :usage true})))]
    (when (str/blank? arg)
      (throw (ex-info (str "Usage: query " cmd " " (:usage (by-name cmd))) {:command cmd :usage true})))
    (let [{:keys [results] :as r} (run ctx arg)]
      (cond-> (assoc r :total (count results))
        limit (update :results #(vec (take limit %)))))))

(defn format-text
  "The results of command `cmd` as text, one result a line."
  [ctx cmd {:keys [results total]}]
  (let [ctx (assoc ctx :lines-of (memoize file-lines))
        more (- (or total (count results)) (count results))]
    (if (seq results)
      (str (str/join "\n" (map #((:line (by-name cmd)) ctx %) results))
           (when (pos? more) (str "\n... " more " more (--limit to see them)")))
      "No results.")))

(defn text
  "Run query command `args` and give its results as text."
  [ctx [cmd :as args] & [opts]]
  (format-text ctx cmd (run ctx args opts)))

(defn help
  "What `query` can do."
  []
  (str "Usage: clojure-lite-lsp query <command> <argument> [--json] [--no-sync] [--project <dir>]\n\n"
       "A <symbol> is ns/name (a var) or ns (a namespace); file:line:col is 1-based.\n\n"
       "Commands:\n"
       (str/join "\n" (for [{:keys [name usage doc]} commands]
                        (format "  %-16s %-26s %s" name usage doc)))
       "\n\nOptions:\n"
       "  --json           results as JSON\n"
       "  --no-sync        don't bring the project's index up to date first\n"
       "  --project <dir>  the project (default: the one containing the current directory)"))
