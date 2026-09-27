# Claude Code

Claude Code has an LSP tool (go to definition, find references, hover, document
and workspace symbols, implementations, incoming and outgoing calls), and takes
language servers from plugins. clojure-lite-lsp answers every one of those
operations.

## Set up

```sh
clojure-lite-lsp setup --agent claude
```

In the project's directory. It:

1. Writes a Claude Code plugin that runs `clojure-lite-lsp lsp` for `.clj`,
   `.cljs`, `.cljc`, `.edn` and `.bb` files, in a marketplace of this machine's
   (`~/.cache/clojure-lite-lsp/claude-marketplace`: a marketplace's name is
   per user).
2. Registers that marketplace (`claude plugin marketplace add`), and installs
   the plugin for the project (`claude plugin install … --scope project`),
   which records it in `.claude/settings.json`.
3. Adds a skill, `.claude/skills/clojure-lite-lsp/SKILL.md`, describing the
   [query commands](../guide/query.md).
4. Adds a short section to `CLAUDE.md` pointing Claude at them instead of grep.
5. Indexes the project.

If `claude` isn't on your `PATH`, it prints the two `claude plugin` commands to
run instead.

## How Claude uses it

- **The LSP tool**, from a file position. The plugin asks the server to wait
  for the project's index before answering (`waitForIndex`, up to two
  minutes), so Claude doesn't get an answer from a half-built index.
- **`clojure-lite-lsp query`**, by symbol name (`app.core/foo`), through the
  skill. Claude tends to reach for this when it knows a name but not a
  position.

For example, asked *which functions call `metabase.util/select-keys-when`*,
Claude ran `clojure-lite-lsp query callers metabase.util/select-keys-when` and
listed every caller with its file and line.

!!! note
    On a tiny project a model may still reach for grep, which works there.
    The instructions make the tools available; they can't force their use.

## Teammates

`.claude/settings.json`, the skill and `CLAUDE.md` can be committed. A teammate
needs the binary on their `PATH` and runs `clojure-lite-lsp setup --agent
claude` once, to register the marketplace on their machine.
