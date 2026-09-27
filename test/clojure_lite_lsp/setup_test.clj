(ns clojure-lite-lsp.setup-test
  "Setting a project up for an agent: the files it needs, the agent's own
  CLI told about the server, the project indexed. Again and again without
  harm."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure-lite-lsp.setup :as setup]
   [clojure-lite-lsp.test-util :as tu]))

(defn slurp-in [dir path] (slurp (io/file dir path)))
(defn json-in [dir path] (json/parse-string (slurp-in dir path)))

(defn fake-cli
  "A stand-in for running agents' CLIs: records [cwd & command], answers
  with `exits` ({command-prefix exit}, default 0)."
  [exits]
  (let [ran (atom [])]
    [ran (fn [cmd cwd]
           (swap! ran conj (into [cwd] cmd))
           {:exit (or (some (fn [[prefix exit]] (when (= prefix (take (count prefix) cmd)) exit)) exits) 0)
            :out ""})]))

(defn run-setup [agent dir & {:keys [exits]}]
  (let [home (str (tu/temp-dir))
        indexed (atom [])
        [ran run] (fake-cli exits)
        result (setup/setup! {:agent agent :dir dir :home home :run run :index! #(swap! indexed conj %)})]
    {:home home :ran @ran :indexed @indexed :result result}))

(deftest claude-gets-the-language-server-and-a-skill
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [home ran indexed]} (run-setup "claude" dir)
        market (str (io/file home "claude-marketplace"))]
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
             (mapv (comp vec rest) (filter #(= "claude" (second %)) (remove #(= "--version" (last %)) ran)))))
      (is (= dir (first (last ran))) "installed from the project's dir"))
    (testing "a skill for the query commands"
      (let [skill (slurp-in dir ".claude/skills/clojure-lite-lsp/SKILL.md")]
        (is (str/starts-with? skill "---\nname: clojure-lite-lsp\ndescription: "))
        (is (str/includes? skill "clojure-lite-lsp query references"))))
    (testing "CLAUDE.md, which Claude Code always reads, points at them"
      (is (str/includes? (slurp-in dir "CLAUDE.md") "clojure-lite-lsp query")))
    (testing "the project indexed, so the first question is answered at once"
      (is (= [dir] indexed)))))

(deftest a-registered-marketplace-is-updated-instead
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "claude" dir :exits {["claude" "plugin" "marketplace" "add"] 1})]
    (is (some #(= ["claude" "plugin" "marketplace" "update" "clojure-lite-lsp"] (vec (rest %))) ran))))

(deftest without-the-agents-cli-it-says-what-to-run
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran result]} (run-setup "claude" dir :exits {["claude" "--version"] 127})]
    (is (not-any? #(= "plugin" (nth % 2 nil)) ran))
    (is (some #(str/includes? % "claude plugin install clojure-lite-lsp@clojure-lite-lsp --scope project")
              (:notes result)))))

(deftest codex-gets-the-mcp-server-and-instructions
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "codex" dir :exits {["codex" "mcp" "get"] 1})]
    (testing "for the project (read once the project is trusted)"
      (is (str/includes? (slurp-in dir ".codex/config.toml")
                         "[mcp_servers.clojure-lite-lsp]\ncommand = \"clojure-lite-lsp\"\nargs = [\"mcp\"]\n")))
    (testing "and for the user, so it works before then (the server finds the project from where Codex runs)"
      (is (some #(= ["codex" "mcp" "add" "clojure-lite-lsp" "--" "clojure-lite-lsp" "mcp"] (vec (rest %))) ran)))
    (is (str/includes? (slurp-in dir "AGENTS.md") "clojure-lite-lsp query references"))))

(deftest codex-already-knowing-the-server-isnt-told-again
  (let [dir (.getCanonicalPath (tu/temp-dir))
        {:keys [ran]} (run-setup "codex" dir)]
    (is (not-any? #(= "add" (nth % 3 nil)) ran))))

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

(deftest the-agent-must-be-one-known
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"claude, codex"
                        (setup/setup! {:agent "emacs" :dir (str (tu/temp-dir)) :home (str (tu/temp-dir))}))))
