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
   [clojure-lite-lsp.java :as java]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.sources :as sources]
   [clojure.java.io :as io]
   [clojure.string :as str])
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

;;;; results

(defn- ->result
  "A location from the query layer as a result, {:path :line :column
  :end-line :end-column}, or nil when it has no file: a jar entry is
  extracted, a Java class found in its sources."
  [{:keys [c p home]} {:keys [entry java-class] :as loc}]
  (let [{:keys [path pos]} (cond
                             java-class (java/class-location c home p java-class)
                             entry (when-let [f (sources/extract! home loc)] {:path f :pos (:pos loc)})
                             :else loc)
        [line col end-line end-col] pos]
    (when (and path line)
      {:path path :line line :column col :end-line end-line :end-column end-col})))

(defn- results-of [ctx locs] (into [] (comp (keep #(->result ctx %)) (distinct)) locs))

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

(defn- display-name
  "How a definition is shown: ns/name for a var, the name of a namespace."
  [{:keys [kind ns name]}]
  (if (= :ns-def kind) name (symbol-name ns name)))

;;;; the commands

(def target-usage
  "The argument of the commands that take a symbol or a position."
  "<symbol | file:line:col>")

(defn- elements-command
  "A command giving the locations (query c p elements & args) finds for
  what its target means."
  [query & args]
  (fn [{:keys [c p] :as ctx} target]
    {:results (results-of ctx (apply query c p (elements ctx target) args))}))

(defn- calls-command
  "A command giving the calls `query` (q/incoming-calls, q/outgoing-calls)
  finds for the vars its target means, each with the function at the
  other end under `other` (:caller, :callee)."
  [query other]
  (fn [{:keys [c p] :as ctx} target]
    {:results (vec (for [[ns name] (vars ctx target)
                         {call-sites :calls who other} (query c p ns name)
                         r (results-of ctx call-sites)]
                     (assoc r other (symbol-name (:ns who) (:name who)))))}))

(defn- location-text [ctx {:keys [path line] :as r}]
  (str (at ctx r) ": " (source-line ctx path line)))

(def commands
  "The query commands: {:name :usage :doc :run :line}, `:run` giving a
  command's results for its argument, `:line` one result as text. The CLI's
  help and dispatch, the MCP tools and the agents' instructions all come
  from this table."
  [{:name "definition"
    :usage target-usage
    :doc "Where a var or namespace is defined."
    :run (elements-command q/definition-of-elements)
    :line location-text}

   {:name "references"
    :usage target-usage
    :doc "Every use of a var, namespace or keyword."
    :run (elements-command q/references-of-elements {})
    :line location-text}

   {:name "implementations"
    :usage target-usage
    :doc "Implementations of a protocol or protocol method, and a multimethod's methods."
    :run (elements-command q/implementations-of-elements)
    :line location-text}

   {:name "doc"
    :usage target-usage
    :doc "A var's or namespace's arglists and docstring."
    :run (fn [{:keys [c p] :as ctx} target]
           {:results (vec (for [{:keys [doc arglists location] :as info} (q/hover-of-elements c p (elements ctx target))]
                            (merge {:symbol (display-name info) :arglists (vec arglists) :doc doc}
                                   (some->> location (->result ctx)))))})
    :line (fn [ctx {:keys [symbol arglists doc path] :as r}]
            (str/join "\n" (filter some? [symbol
                                          (when (seq arglists) (str/join " " arglists))
                                          doc
                                          (when path (at ctx r))])))}

   {:name "callers"
    :usage target-usage
    :doc "Where a function is called, and from which function."
    :run (calls-command q/incoming-calls :caller)
    :line (fn [ctx r] (str (at ctx r) ": " (:caller r) " calls it"))}

   {:name "callees"
    :usage target-usage
    :doc "What a function calls, and where."
    :run (calls-command q/outgoing-calls :callee)
    :line (fn [ctx r] (str (at ctx r) ": calls " (:callee r)))}

   {:name "symbols"
    :usage "<text>"
    :doc "Definitions whose name contains the text, exact names first, then the project's own."
    :run (fn [{:keys [c p] :as ctx} text]
           {:results (vec (for [{:keys [kind location] :as sym} (q/workspace-symbols c p text {:limit 50})
                                :let [r (->result ctx location)]
                                :when r]
                            (assoc r :symbol (display-name sym) :kind (name kind))))})
    :line (fn [ctx r] (str (at ctx r) ": " (:symbol r)))}

   {:name "outline"
    :usage "<file>"
    :doc "The namespaces and vars a file defines, in order."
    :run (fn [{:keys [c p] :as ctx} file]
           (let [path (file-of ctx file)]
             {:results (vec (for [{:keys [kind pos] :as sym} (q/document-symbols c p path)]
                              (assoc (->result ctx {:path path :pos pos})
                                     :kind (name kind)
                                     :symbol (display-name sym))))}))
    :line (fn [ctx {:keys [kind symbol] :as r}]
            (str (at ctx r) ": " (if (= "ns-def" kind) (str "ns " symbol) symbol)))}])

(def ^:private by-name (into {} (map (juxt :name identity)) commands))

(defn run
  "Run query command `args` ([name arg]) in `ctx` ({:c :p :root :home
  :cwd}): {:results [...] :total n}, at most `limit` results."
  ([ctx args] (run ctx args {}))
  ([ctx [cmd arg] {:keys [limit]}]
   (let [{:keys [usage] run-command :run} (or (by-name cmd)
                                              (throw (ex-info (str "Unknown query command: " cmd ". Commands: "
                                                                   (str/join ", " (map :name commands)))
                                                              {:command cmd :usage true})))]
     (when (str/blank? arg)
       (throw (ex-info (str "Usage: query " cmd " " usage) {:command cmd :usage true})))
     (let [{:keys [results] :as r} (run-command ctx arg)]
       (cond-> (assoc r :total (count results))
         limit (update :results #(vec (take limit %))))))))

(defn format-text
  "The results of command `cmd` as text, one result a line."
  [ctx cmd {:keys [results total]}]
  (let [ctx (assoc ctx :lines-of (memoize file-lines))
        more (- (or total (count results)) (count results))]
    (if (seq results)
      (str (str/join "\n" (map #((:line (by-name cmd)) ctx %) results))
           (when (pos? more) (str "\n... " more " more (--limit to see them)")))
      "No results.")))

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
