(ns clojure-lite-lsp.setup-test
  "Setting a project up for an agent: the files it needs, the agent's own
  CLI told about the server, the project indexed. Again and again without
  harm."
  (:require
   [cheshire.core :as json]
   [clojure-lite-lsp.setup :as setup]
   [clojure-lite-lsp.test-util :as tu]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn slurp-in [dir path] (slurp (io/file dir path)))
(defn json-in [dir path] (json/parse-string (slurp-in dir path)))

(defn fake-cli
  "A stand-in for running agents' CLIs: records {:cwd :cmd}, answers with
  `exits` ({command-prefix exit}, default 0)."
  [exits]
  (let [ran (atom [])]
    [ran (fn [cmd cwd]
           (swap! ran conj {:cwd cwd :cmd (vec cmd)})
           {:exit (or (some (fn [[prefix exit]] (when (= prefix (take (count prefix) cmd)) exit)) exits) 0)
            :out ""})]))

(defn run-setup [agent dir & {:keys [exits]}]
  (let [data-dir (str (tu/temp-dir))
        indexed (atom [])
        [ran run] (fake-cli exits)
        result (setup/setup! {:agent agent :dir dir :data-dir data-dir :run run :index! #(swap! indexed conj %)})]
    {:data-dir data-dir :ran @ran :indexed @indexed :result result}))

(deftest claude-gets-the-language-server-and-a-skill
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [data-dir ran indexed]} (run-setup "claude" dir)
        market (str (io/file data-dir "claude-marketplace"))]
    (testing "the plugin, in a marketplace of this machine's (a marketplace's name is per user)"
      (is (= {"clojure-lite-lsp" {"command" "clojure-lite-lsp" "args" ["lsp"]
                                  "extensionToLanguage" {".bb" "clojure" ".clj" "clojure" ".cljc" "clojure"
                                                         ".cljs" "clojure" ".edn" "clojure"}
                                  "initializationOptions" {"waitForIndex" true}
                                  "diagnostics" false}}
             (json-in market "plugin/.lsp.json")))
      (is (= "./plugin" (get-in (json-in market ".claude-plugin/marketplace.json") ["plugins" 0 "source"]))))
    (testing "registered with Claude Code, and installed for the project"
      (is (= [["claude" "plugin" "marketplace" "add" market]
              ["claude" "plugin" "install" "clojure-lite-lsp@clojure-lite-lsp" "--scope" "project"]]
             (into [] (comp (map :cmd) (filter #(= "claude" (first %))) (remove #(= "--version" (last %)))) ran)))
      (is (= dir (:cwd (last ran))) "installed from the project's dir"))
    (testing "a skill for the query commands"
      (let [skill (slurp-in dir ".claude/skills/clojure-lite-lsp/SKILL.md")]
        (is (str/starts-with? skill "---\nname: clojure-lite-lsp\ndescription: "))
        (is (str/includes? skill "clojure-lite-lsp query references"))))
    (testing "AGENTS.md, which Claude Code reads, points at them; no CLAUDE.md"
      (is (str/includes? (slurp-in dir "AGENTS.md") "clojure-lite-lsp query"))
      (is (not (.exists (io/file dir "CLAUDE.md")))))
    (testing "AGENTS.md is read every session: a nudge, not the reference (that's the skill)"
      (let [md (slurp-in dir "AGENTS.md")]
        (is (not (str/includes? md "| Command")))
        (is (< (count (str/split-lines md)) 10))))
    (testing "the project indexed, so the first question is answered at once"
      (is (= [dir] indexed)))))

(deftest a-registered-marketplace-is-updated-instead
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "claude" dir :exits {["claude" "plugin" "marketplace" "add"] 1})]
    (is (some #(= ["claude" "plugin" "marketplace" "update" "clojure-lite-lsp"] (:cmd %)) ran))))

(deftest without-the-agents-cli-it-says-what-to-run
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran result]} (run-setup "claude" dir :exits {["claude" "--version"] 127})]
    (is (not-any? #(= ["claude" "plugin"] (take 2 (:cmd %))) ran))
    (is (some #(str/includes? % "claude plugin install clojure-lite-lsp@clojure-lite-lsp --scope project")
              (:notes result)))
    (is (every? string? (:notes result)) "only notes, no blanks")))

(deftest codex-gets-the-mcp-server-and-instructions
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "codex" dir :exits {["codex" "mcp" "get"] 1})]
    (testing "for the project (read once the project is trusted)"
      (is (str/includes? (slurp-in dir ".codex/config.toml")
                         "[mcp_servers.clojure-lite-lsp]\ncommand = \"clojure-lite-lsp\"\nargs = [\"mcp\"]\n")))
    (testing "and for the user, so it works before then (the server finds the project from where Codex runs)"
      (is (some #(= ["codex" "mcp" "add" "clojure-lite-lsp" "--" "clojure-lite-lsp" "mcp"] (:cmd %)) ran)))
    (is (str/includes? (slurp-in dir "AGENTS.md") "MCP"))))

(deftest codex-already-knowing-the-server-isnt-told-again
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "codex" dir)]
    (is (not-any? #(= ["codex" "mcp" "add"] (take 3 (:cmd %))) ran))))

(deftest setting-up-again-keeps-what-was-there
  (let [dir (.getCanonicalPath (tu/temp-dir))]
    (io/make-parents (io/file dir ".codex/config.toml"))
    (spit (io/file dir ".codex/config.toml") "model = \"o4\"\n\n[mcp_servers.other]\ncommand = \"other\"\n")
    (spit (io/file dir "AGENTS.md") "# Agents\n\nBe nice.\n")
    (dotimes [_ 2]
      (run-setup "claude" dir)
      (run-setup "codex" dir))
    (let [toml (slurp-in dir ".codex/config.toml")]
      (is (str/includes? toml "model = \"o4\""))
      (is (str/includes? toml "[mcp_servers.other]\ncommand = \"other\""))
      (is (= 1 (count (re-seq #"\[mcp_servers\.clojure-lite-lsp\]" toml))) "once, however often it runs"))
    (let [md (slurp-in dir "AGENTS.md")]
      (is (str/starts-with? md "# Agents\n\nBe nice.\n"))
      (is (= 1 (count (re-seq #"clojure-lite-lsp:start" md)))))))

(deftest claude-and-codex-share-one-agents-md-section
  (let [dir (.getCanonicalPath (tu/temp-dir))]
    (run-setup "claude" dir)
    (let [after-claude (slurp-in dir "AGENTS.md")]
      (run-setup "codex" dir)
      (is (= after-claude (slurp-in dir "AGENTS.md")) "the same section, whichever agent wrote it")
      (is (str/includes? after-claude "LSP tool") "Claude Code's way in")
      (is (str/includes? after-claude "MCP") "and Codex's"))))

(deftest the-agent-must-be-one-known
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"claude, codex"
                        (setup/setup! {:agent "emacs" :dir (str (tu/temp-dir)) :data-dir (str (tu/temp-dir))}))))

(defn codex-toml [before]
  (let [dir (.getCanonicalPath (tu/temp-dir))]
    (io/make-parents (io/file dir ".codex/config.toml"))
    (spit (io/file dir ".codex/config.toml") before)
    (run-setup "codex" dir)
    (slurp-in dir ".codex/config.toml")))

(deftest codex-config-edits-are-careful
  (testing "a quoted header, a trailing comment: found, not duplicated; the user's keys kept"
    (doseq [header ["[mcp_servers.\"clojure-lite-lsp\"]" "[mcp_servers.clojure-lite-lsp]  # ours"]]
      (let [toml (codex-toml (str header "\ncommand = \"old\"\ntool_timeout_sec = 120\n"))]
        (is (= 1 (count (re-seq #"(?m)^\[mcp_servers\." toml))) header)
        (is (str/includes? toml "tool_timeout_sec = 120"))
        (is (str/includes? toml "command = \"clojure-lite-lsp\""))
        (is (str/includes? toml "args = [\"mcp\"]")))))
  (testing "defined inline under [mcp_servers]: left as it is"
    (let [before "[mcp_servers]\nclojure-lite-lsp = { command = \"x\", args = [\"mcp\"] }\n"]
      (is (= before (codex-toml before)))))
  (testing "a multi-line array in the table after is kept whole"
    (let [toml (codex-toml "[mcp_servers.clojure-lite-lsp]\ncommand = \"old\"\n\n[other]\nlist = [\n  [\"a\"],\n  [\"b\"],\n]\n")]
      (is (str/includes? toml "[other]\nlist = [\n  [\"a\"],\n  [\"b\"],\n]\n"))))
  (testing "a literal string ending in a backslash (no escapes in '...')"
    (let [toml (codex-toml "paths = ['C:\\', 'd']\n\n[mcp_servers.clojure-lite-lsp]\ncommand = \"old\"\n")]
      (is (= 1 (count (re-seq #"(?m)^\[mcp_servers\." toml))) toml))))

(deftest a-damaged-section-is-left-to-the-user
  (let [dir (.getCanonicalPath (tu/temp-dir))
        damaged "# Agents\n<!-- clojure-lite-lsp:start -->\nmine\n"]
    (spit (io/file dir "AGENTS.md") damaged)
    (let [{:keys [result]} (run-setup "codex" dir)]
      (is (= damaged (slurp-in dir "AGENTS.md")))
      (is (some #(str/includes? % "AGENTS.md") (:notes result))))))

(deftest the-directory-must-exist
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not a directory"
                        (setup/setup! {:agent "codex" :dir "/no/such/dir" :data-dir (str (tu/temp-dir))}))))

(deftest setup-parses-its-arguments
  (is (= {:agents ["claude" "codex"] :dir "x" :index? false}
         (setup/parse-args ["--agent" "claude,codex" "x" "--no-index"])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown option" (setup/parse-args ["--agent" "claude" "--no-sync"])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"one directory" (setup/parse-args ["--agent" "claude" "a" "b"]))))

(deftest a-failed-install-isnt-reported-as-written
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [result]} (run-setup "claude" dir :exits {["claude" "plugin" "install"] 1})]
    (is (not (some #{".claude/settings.json"} (:wrote result))))))

(deftest agent-clis-dont-hang-setup
  (testing "stdin is closed: a command reading it ends"
    (is (= 0 (:exit (setup/run-command ["cat"] (str (tu/temp-dir)))))))
  (testing "a command that doesn't end is stopped"
    (let [start (System/currentTimeMillis)]
      (is (not= 0 (:exit (setup/run-command ["sleep" "10"] (str (tu/temp-dir)) {:timeout-ms 300}))))
      (is (< (- (System/currentTimeMillis) start) 5000)))))
