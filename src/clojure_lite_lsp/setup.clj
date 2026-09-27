(ns clojure-lite-lsp.setup
  "`clojure-lite-lsp setup --agent <agent> [dir]`: make a project ready for
  a coding agent. Safe to run again: other settings are kept, and its own
  parts are replaced, not repeated.

  - claude: Claude Code takes language servers only from plugins, and a
    plugin only from a registered marketplace. The plugin lives in a
    marketplace of this machine's (<home>/claude-marketplace: a
    marketplace's name is per user), registered and installed for the
    project with the `claude` CLI, which records it in the project's
    .claude/settings.json. Plus a skill for the query commands
    (.claude/skills/clojure-lite-lsp/SKILL.md).
  - codex: Codex has no language server support: the query commands as an
    MCP server, in the project's .codex/config.toml (read once the project
    is trusted) and for the user with `codex mcp add` (the server finds the
    project from where Codex runs), and a section in AGENTS.md.

  Then the project is indexed, so the agent's first question is answered
  at once."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-lite-lsp.commands :as commands]
   [clojure-lite-lsp.version :as version]))

(set! *warn-on-reflection* true)

(def binary "clojure-lite-lsp")

(def ^:private author {:name "Nathan Voxland" :email "nathan@voxland.net"})

(def ^:private extensions [".clj" ".cljs" ".cljc" ".edn" ".bb"])

(defn- instructions
  "How an agent uses the query commands, as Markdown."
  [agent]
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
                   (str "| `" name "` | `" usage "` | " doc " |\n")))
       (case agent
         "claude" (str "\nThe LSP tool answers the same for Clojure files (goToDefinition, findReferences,\n"
                       "hover, incomingCalls, ...), from a file position.\n")
         "codex" "\nThe same commands are tools of the `clojure-lite-lsp` MCP server.\n")))

;;;; writing, idempotently

(defn- write! [dir path content]
  (let [f (io/file dir path)]
    (io/make-parents f)
    (spit f content)
    path))

(defn- json-str [x] (str (json/generate-string x {:pretty true}) "\n"))

(defn- merge-json!
  "Merge `f` into the JSON object in `path` (created if missing)."
  [dir path f]
  (let [file (io/file dir path)
        m (if (.isFile file) (json/parse-string (slurp file)) {})]
    (write! dir path (json-str (f m)))))

(def ^:private start-marker "<!-- clojure-lite-lsp:start -->")
(def ^:private end-marker "<!-- clojure-lite-lsp:end -->")

(defn- replace-section
  "`text` with its marked clojure-lite-lsp section replaced by `section`,
  or `section` appended."
  [text section]
  (let [block (str start-marker "\n" section end-marker "\n")
        start (str/index-of text start-marker)
        end (some-> (str/index-of text end-marker) (+ (count end-marker)))]
    (if (and start end)
      (str (subs text 0 start) (str/trim-newline block) (subs text end))
      (str text (when (and (seq text) (not (str/ends-with? text "\n\n"))) (if (str/ends-with? text "\n") "\n" "\n\n"))
           block))))

(defn- toml-table
  "`toml` with table `header` ([a.b]) replaced by `body`, or appended."
  [toml header body]
  (let [lines (str/split-lines toml)
        start (.indexOf ^java.util.List lines header)]
    (if (neg? start)
      (str toml (when (and (seq toml) (not (str/ends-with? toml "\n"))) "\n")
           (when (seq toml) "\n") header "\n" body)
      (let [end (or (some #(when (str/starts-with? (str/trim (nth lines %)) "[") %)
                          (range (inc start) (count lines)))
                    (count lines))
            rest-lines (drop end lines)]
        (str (str/join "\n" (take start lines)) (when (pos? start) "\n")
             header "\n" body
             (when (seq rest-lines) (str "\n" (str/join "\n" rest-lines) "\n")))))))

;;;; agents' CLIs

(defn run-command
  "Run `cmd` in `cwd`: {:exit :out}; exit 127 when it can't be started."
  [cmd cwd]
  (try
    (let [p (-> (ProcessBuilder. ^java.util.List (vec cmd))
                (.directory (io/file cwd))
                (.redirectErrorStream true)
                (.start))
          out (slurp (.getInputStream p))]
      {:exit (.waitFor p) :out out})
    (catch java.io.IOException e {:exit 127 :out (ex-message e)})))

(defn- has-cli? [run cli dir] (zero? (:exit (run [cli "--version"] dir))))

;;;; agents

(defn- claude! [{:keys [dir home run]}]
  (let [market (str (io/file home "claude-marketplace"))
        description "Clojure code navigation from clojure-lite-lsp"
        plugin-id (str binary "@" binary)
        install ["claude" "plugin" "install" plugin-id "--scope" "project"]]
    (write! market "plugin/.claude-plugin/plugin.json"
            (json-str {:name binary :version version/version :description description :author author}))
    (write! market "plugin/.lsp.json"
            (json-str {binary {:command binary :args ["lsp"]
                               :extensionToLanguage (into (sorted-map) (map (fn [e] [e "clojure"])) extensions)
                               ;; an agent asks once and trusts the answer: not
                               ;; from a half-built index
                               :initializationOptions {:waitForIndex true}
                               ;; it has none: don't wait for them after edits
                               :diagnostics false}}))
    (write! market ".claude-plugin/marketplace.json"
            (json-str {:name binary :owner author :description description
                       :plugins [{:name binary :source "./plugin" :description description}]}))
    (let [claude-md (write! dir "CLAUDE.md"
                            (replace-section (let [f (io/file dir "CLAUDE.md")] (if (.isFile f) (slurp f) ""))
                                             (str "## Navigating Clojure code\n\n"
                                                  "To find where Clojure code is defined, used or called, use "
                                                  "`clojure-lite-lsp query` (the clojure-lite-lsp skill) or the LSP tool, "
                                                  "not grep: they resolve aliases, refers and macros. "
                                                  "`clojure-lite-lsp query` lists the commands.\n")))
          skill (write! dir ".claude/skills/clojure-lite-lsp/SKILL.md"
                        (str "---\nname: " binary "\n"
                             "description: Use whenever you need where a Clojure var, function, macro or namespace "
                             "is defined, used, called or implemented, or what a function calls: run "
                             "`clojure-lite-lsp query` instead of grep. It resolves aliases, refers and macros; "
                             "grep misses them and matches strings and comments.\n---\n\n"
                             "# Navigating Clojure code\n\n" (instructions "claude")))]
      (if (has-cli? run "claude" dir)
        (do (when-not (zero? (:exit (run ["claude" "plugin" "marketplace" "add" market] dir)))
              ;; registered before: take this version's plugin
              (run ["claude" "plugin" "marketplace" "update" binary] dir))
            (let [{:keys [exit out]} (run install dir)]
              {:wrote [skill claude-md ".claude/settings.json"]
               :notes (when-not (zero? exit) [(str "Installing the plugin failed: " out)])}))
        {:wrote [skill claude-md]
         :notes [(str "claude isn't on PATH. With it, run in " dir ":\n"
                      "  claude plugin marketplace add " market "\n"
                      "  " (str/join " " install))]}))))

(defn- codex! [{:keys [dir run]}]
  (let [config ".codex/config.toml"
        f (io/file dir config)
        agents (io/file dir "AGENTS.md")
        add ["codex" "mcp" "add" binary "--" binary "mcp"]
        wrote [(write! dir config
                       (toml-table (if (.isFile f) (slurp f) "")
                                   (str "[mcp_servers." binary "]")
                                   (str "command = \"" binary "\"\nargs = [\"mcp\"]\n")))
               (write! dir "AGENTS.md"
                       (replace-section (if (.isFile agents) (slurp agents) "")
                                        (str "## Navigating Clojure code\n\n" (instructions "codex"))))]]
    (cond
      (not (has-cli? run "codex" dir))
      {:wrote wrote :notes [(str "codex isn't on PATH. With it, run: " (str/join " " add))]}

      (zero? (:exit (run ["codex" "mcp" "get" binary] dir)))
      {:wrote wrote}

      :else
      (let [{:keys [exit out]} (run add dir)]
        {:wrote wrote :notes (when-not (zero? exit) [(str "codex mcp add failed: " out)])}))))

(def agents
  "The agents `setup` knows, and what it does for each."
  {"claude" claude! "codex" codex!})

(defn setup!
  "Make project `dir` ready for `agent`: {:wrote [paths relative to dir]
  :notes [for the user]}. `home` is clojure-lite-lsp's home dir; `run`
  runs agents' CLIs (run-command); `index!` indexes the project."
  [{:keys [agent dir home run index!] :or {run run-command}}]
  (let [f (or (agents agent)
              (throw (ex-info (str "Unknown agent: " agent ". Agents: " (str/join ", " (sort (keys agents))))
                              {:agent agent})))
        dir (.getCanonicalPath (io/file dir))
        result (f {:dir dir :home home :run run})]
    (when index! (index! dir))
    result))
