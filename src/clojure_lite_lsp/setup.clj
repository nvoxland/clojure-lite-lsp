(ns clojure-lite-lsp.setup
  "`clojure-lite-lsp setup --agent <agent> [dir]`: make a project ready for
  a coding agent. Safe to run again: other settings are kept, and its own
  parts are replaced, not repeated.

  - claude: Claude Code takes language servers only from plugins, and a
    plugin only from a registered marketplace. The plugin lives in a
    marketplace of this machine's (<data dir>/claude-marketplace: a
    marketplace's name is per user), registered and installed for the
    project with the `claude` CLI, which records it in the project's
    .claude/settings.json. Plus a skill for the query commands
    (.claude/skills/clojure-lite-lsp/SKILL.md), and a section in AGENTS.md.
  - codex: Codex has no language server support: the query commands as an
    MCP server, in the project's .codex/config.toml (read once the project
    is trusted) and for the user with `codex mcp add` (the server finds the
    project from where Codex runs), and a section in AGENTS.md.

  Then the project is indexed, so the agent's first question is answered
  at once."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.process :as process]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.util.regex Pattern]))

(set! *warn-on-reflection* true)

(def ^:private binary "clojure-lite-lsp")

(def ^:private author {:name "Nathan Voxland" :email "nathan@voxland.net"})

(def ^:private extensions [".clj" ".cljs" ".cljc" ".edn" ".bb"])

(defn- query-instructions
  "How an agent uses the query commands, as Markdown."
  []
  (str "Use `clojure-lite-lsp query` to navigate Clojure code: it knows namespaces, aliases,\n"
       "refers and macros, so it finds what grep misses (and doesn't match strings or\n"
       "comments). Prefer it to grep for definitions, uses and callers.\n\n"
       "```sh\n"
       "clojure-lite-lsp query references app.core/foo   # every use of a var\n"
       "clojure-lite-lsp query definition src/app/core.clj:12:5\n"
       "clojure-lite-lsp query callers app.core/foo\n"
       "clojure-lite-lsp query                           # all commands\n"
       "```\n\n"
       "A target is `ns/name` (a var), `ns` (a namespace) or `file:line:col` (1-based).\n"
       "Answers are `path:line:col: text` lines; `--json` for data. The project's index\n"
       "is brought up to date first; the first time, a big project takes a minute.\n\n"
       "| Command | Argument | |\n|---|---|---|\n"
       (str/join (for [{:keys [name usage doc]} commands/commands]
                   (str "| `" name "` | `" usage "` | " doc " |\n")))))

(def ^:private claude-lsp-note
  (str "In Claude Code, the LSP tool answers the same for Clojure files (goToDefinition,\n"
       "findReferences, hover, incomingCalls, ...), from a file position.\n"))

(def ^:private agents-md-section
  "AGENTS.md's section: the same whichever agents are set up, so setting up
  another replaces it rather than adding a second. Agents read it every
  session, so it's a nudge; the reference is Claude Code's skill, and the
  MCP tools' own descriptions."
  (str "## Navigating Clojure code\n\n"
       "To find where Clojure code is defined, used or called, use the LSP tool (Claude\n"
       "Code), the `clojure-lite-lsp` MCP tools (Codex) or `clojure-lite-lsp query` (it\n"
       "lists its commands), not grep: they resolve aliases, refers and macros.\n"))

;;;; writing, idempotently

(defn- write! [dir path content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)
    path))

(defn- json-str [x] (str (json/generate-string x {:pretty true}) "\n"))

(def ^:private start-marker "<!-- clojure-lite-lsp:start -->")
(def ^:private end-marker "<!-- clojure-lite-lsp:end -->")

(defn- occurrences [s sub] (count (re-seq (re-pattern (Pattern/quote sub)) s)))

(defn- section-bounds
  "[start end] of `text`'s marked clojure-lite-lsp section, when it has
  exactly one, in order."
  [text]
  (let [start (str/index-of text start-marker)
        end (some-> (str/index-of text end-marker) (+ (count end-marker)))]
    (when (and (= 1 (occurrences text start-marker) (occurrences text end-marker)) (< start end))
      [start end])))

(defn- replace-section
  "[text note]: `text` with its marked clojure-lite-lsp section replaced by
  `section`, or `section` appended. A damaged section (a marker missing or
  repeated, or out of order) is left for the user: [text note]."
  [text section file]
  (let [block (str start-marker "\n" section end-marker "\n")]
    (cond
      (= 0 (occurrences text start-marker) (occurrences text end-marker))
      [(str text (when (and (seq text) (not (str/ends-with? text "\n\n"))) (if (str/ends-with? text "\n") "\n" "\n\n"))
            block)
       nil]

      (section-bounds text)
      (let [[start end] (section-bounds text)]
        [(str (subs text 0 start) (str/trim-newline block) (subs text end)) nil])

      :else
      [text (str file "'s clojure-lite-lsp section is damaged (its " start-marker " / " end-marker
                 " markers): fix or remove it, then run setup again")])))

;;;; .codex/config.toml

(defn- depth-change
  "How many arrays `line` opens minus closes, outside strings and comments."
  [line]
  (loop [[c & more] (seq line) depth 0 in-string nil]
    (cond
      (nil? c) depth
      ;; only "..." has escapes: '...' is literal
      in-string (cond (and (= \" in-string) (= \\ c)) (recur (next more) depth in-string)
                      (= in-string c) (recur more depth nil)
                      :else (recur more depth in-string))
      (#{\" \'} c) (recur more depth c)
      (= \# c) depth
      (= \[ c) (recur more (inc depth) nil)
      (= \] c) (recur more (dec depth) nil)
      :else (recur more depth nil))))

(def ^:private header-re #"\s*\[\[?\s*[^\[\]]+\]\]?\s*(#.*)?")

(defn- ours-header? [line]
  (re-matches #"\s*\[\s*mcp_servers\s*\.\s*(\"clojure-lite-lsp\"|clojure-lite-lsp)\s*\]\s*(#.*)?" line))

(defn- statements
  "`toml`'s lines grouped into statements (a multi-line array is one), each
  {:lines [...] :header? bool :key k}."
  [toml]
  (loop [[line & more] (str/split-lines toml) depth 0 current nil out []]
    (if (nil? line)
      (cond-> out current (conj current))
      (let [continuing? (pos? depth)
            depth' (+ depth (if (and (not continuing?) (re-matches header-re line)) 0 (depth-change line)))]
        (if continuing?
          (recur more depth' (update current :lines conj line) out)
          (let [stmt {:lines [line]
                      :header? (boolean (re-matches header-re line))
                      :key (second (re-find #"^\s*(\"[^\"]+\"|[A-Za-z0-9_.-]+)\s*=" line))}]
            (recur more depth' stmt (cond-> out current (conj current)))))))))

(defn- codex-table
  "[toml note]: `toml` with the clojure-lite-lsp MCP server's command and
  args set, in its table (other keys kept) or a new one. Defined some other
  way (an inline table, dotted keys): left as it is, with a note."
  [toml]
  (let [stmts (statements toml)
        body ["command = \"clojure-lite-lsp\"" "args = [\"mcp\"]"]
        text (fn [stmts] (str (str/join "\n" (mapcat :lines stmts)) "\n"))
        ours (first (keep-indexed #(when (and (:header? %2) (ours-header? (first (:lines %2)))) %1) stmts))
        elsewhere? (some #(and (not (:header? %)) (:key %)
                               (re-find #"clojure-lite-lsp" (:key %)))
                         stmts)]
    (cond
      ours
      (let [[before [header & after]] (split-at ours stmts)
            [table rest-stmts] (split-with (complement :header?) after)
            kept (remove #(#{"command" "args"} (:key %)) table)]
        [(text (concat before [header] (map (fn [l] {:lines [l]}) body) kept rest-stmts)) nil])

      elsewhere?
      [toml "the clojure-lite-lsp MCP server is already defined in .codex/config.toml (not as its own table): left as it is"]

      :else
      [(str toml (when (and (seq toml) (not (str/ends-with? toml "\n"))) "\n") (when (seq toml) "\n")
            "[mcp_servers.clojure-lite-lsp]\n" (str/join "\n" body) "\n")
       nil])))

;;;; agents' CLIs

(defn run-command
  "Run `cmd` in `cwd`: {:exit :out}, its output and errors together
  (`process/run`)."
  ([cmd cwd] (run-command cmd cwd {}))
  ([cmd cwd opts]
   (let [{:keys [exit out err]} (process/run cmd (assoc opts :dir cwd :merge-err? true))]
     {:exit exit :out (str out err)})))

(defn- has-cli? [run cli dir] (zero? (:exit (run [cli "--version"] dir))))

;;;; agents

(def ^:private plugin-description "Clojure code navigation from clojure-lite-lsp")

(defn- write-marketplace!
  "Write the Claude Code plugin marketplace with this server into
  `data-dir`: its directory."
  [data-dir]
  (let [market (str (io/file data-dir "claude-marketplace"))]
    (write! market "plugin/.claude-plugin/plugin.json"
            (json-str {:name binary :version version/version :description plugin-description :author author}))
    (write! market "plugin/.lsp.json"
            (json-str {binary {:command binary :args ["lsp"]
                               :extensionToLanguage (into (sorted-map) (map (fn [e] [e "clojure"])) extensions)
                               ;; an agent asks once and trusts the answer: not
                               ;; from a half-built index
                               :initializationOptions {:waitForIndex true}
                               ;; it has none: don't wait for them after edits
                               :diagnostics false}}))
    (write! market ".claude-plugin/marketplace.json"
            (json-str {:name binary :owner author :description plugin-description
                       :plugins [{:name binary :source "./plugin" :description plugin-description}]}))
    market))

(defn- skill-md []
  (str "---\nname: " binary "\n"
       "description: Use whenever you need where a Clojure var, function, macro or namespace "
       "is defined, used, called or implemented, or what a function calls: run "
       "`clojure-lite-lsp query` instead of grep. It resolves aliases, refers and macros; "
       "grep misses them and matches strings and comments.\n---\n\n"
       "# Navigating Clojure code\n\n" (query-instructions) "\n" claude-lsp-note))

(defn- slurp-if-exists [f] (if (.isFile (io/file f)) (slurp f) ""))

(defn- write-agents-md!
  "Put `agents-md-section` in the project's AGENTS.md: [the path written, or
  nil, and a note, or nil]."
  [dir]
  (let [[text note] (replace-section (slurp-if-exists (io/file dir "AGENTS.md")) agents-md-section "AGENTS.md")]
    [(when-not note (write! dir "AGENTS.md" text)) note]))

(defn- claude! [{:keys [dir data-dir run]}]
  (let [market (write-marketplace! data-dir)
        install ["claude" "plugin" "install" (str binary "@" binary) "--scope" "project"]
        [agents-md agents-md-note] (write-agents-md! dir)
        skill (write! dir ".claude/skills/clojure-lite-lsp/SKILL.md" (skill-md))
        notes (filterv some? [agents-md-note])]
    (if (has-cli? run "claude" dir)
      (do (when-not (zero? (:exit (run ["claude" "plugin" "marketplace" "add" market] dir)))
            ;; registered before: take this version's plugin
            (run ["claude" "plugin" "marketplace" "update" binary] dir))
          (let [{:keys [exit out]} (run install dir)
                installed? (zero? exit)]
            {:wrote (filterv some? [skill agents-md (when installed? ".claude/settings.json")])
             :notes (cond-> notes (not installed?) (conj (str "Installing the plugin failed: " out)))}))
      {:wrote (filterv some? [skill agents-md])
       :notes (conj notes (str "claude isn't on PATH. With it, run in " dir ":\n"
                               "  claude plugin marketplace add " market "\n"
                               "  " (str/join " " install)))})))

(defn- codex! [{:keys [dir run]}]
  (let [config ".codex/config.toml"
        add ["codex" "mcp" "add" binary "--" binary "mcp"]
        [toml toml-note] (codex-table (slurp-if-exists (io/file dir config)))
        [agents-md md-note] (write-agents-md! dir)
        wrote (filterv some? [(when-not toml-note (write! dir config toml)) agents-md])
        notes (filterv some? [toml-note md-note])]
    (cond
      (not (has-cli? run "codex" dir))
      {:wrote wrote :notes (conj notes (str "codex isn't on PATH. With it, run: " (str/join " " add)))}

      (zero? (:exit (run ["codex" "mcp" "get" binary] dir)))
      {:wrote wrote :notes notes}

      :else
      (let [{:keys [exit out]} (run add dir)]
        {:wrote wrote :notes (cond-> notes (not (zero? exit)) (conj (str "codex mcp add failed: " out)))}))))

(def agents
  "The agents `setup` knows, and what it does for each."
  {"claude" claude! "codex" codex!})

(defn setup!
  "Make project `dir` ready for `agent`: {:wrote [paths relative to dir]
  :notes [for the user]}. `data-dir` is clojure-lite-lsp's data dir; `run`
  runs agents' CLIs (run-command); `index!` indexes the project."
  [{:keys [agent dir data-dir run index!] :or {run run-command}}]
  (let [configure! (or (agents agent)
                       (throw (ex-info (str "Unknown agent: " agent ". Agents: " (str/join ", " (sort (keys agents))))
                                       {:agent agent})))
        _ (when-not (.isDirectory (io/file dir))
            (throw (ex-info (str "Not a directory: " dir) {:dir dir})))
        dir (.getCanonicalPath (io/file dir))
        result (configure! {:dir dir :data-dir data-dir :run run})]
    (when index! (index! dir))
    result))

(defn parse-args
  "`setup`'s arguments: {:agents [...] :dir :index?}."
  [args]
  (let [{:keys [agents dirs index?]}
        (loop [[a & more] args acc {:agents [] :dirs [] :index? true}]
          (cond
            (nil? a) acc
            (= "--agent" a) (recur (rest more) (update acc :agents into (remove str/blank? (str/split (str (first more)) #","))))
            (= "--no-index" a) (recur more (assoc acc :index? false))
            (str/starts-with? a "--") (throw (ex-info (str "Unknown option: " a) {:usage true}))
            :else (recur more (update acc :dirs conj a))))]
    (when (> (count dirs) 1)
      (throw (ex-info (str "setup takes one directory, not " (count dirs)) {:usage true})))
    {:agents agents :dir (first dirs) :index? index?}))
