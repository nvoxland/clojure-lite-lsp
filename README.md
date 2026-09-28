# clojure-lite-lsp

A Clojure language server small enough to run in every project and worktree
you have open.

Working with coding agents means many codebases open at once: several
projects, several worktrees of each, with an agent or an editor in every one.
clojure-lite-lsp keeps the cost of each one low:

- **About 20 MB per editor**, whatever the project's size. Indexing happens
  in one background process for the whole machine, which exits when idle.
- **Libraries are indexed once** for the machine, and shared by every project
  that uses them.
- **A new worktree indexes only what differs** from what's already indexed:
  a few seconds for a worktree of a large project.

It stays that small by implementing only the reading side of the language
server protocol. With an agent making the edits, your part is mostly reading
and reviewing code, so it provides:

- go to definition and declaration, find references, implementations
- hover docs, file outlines, symbol search
- call hierarchy (callers and callees)
- occurrence highlighting and argument hints

and leaves out the features for writing code by hand: completion,
diagnostics, formatting, rename and refactorings.

**Documentation: <https://nvoxland.github.io/clojure-lite-lsp/>**. How it
works: [Architecture](https://nvoxland.github.io/clojure-lite-lsp/architecture/).
Building and developing it: [DEV.md](DEV.md).

## Install

Download a binary for macOS or Linux from
[Releases](https://github.com/nvoxland/clojure-lite-lsp/releases) and put it
on your PATH ([details](https://nvoxland.github.io/clojure-lite-lsp/getting-started/install/)).
To build it from source, see [DEV.md](DEV.md).

## Coding agents

```sh
clojure-lite-lsp setup --agent claude   # or codex, or claude,codex; in the project's directory
```

- **Claude Code**:
  - Installs a plugin that gives Claude's LSP tool this server, recorded in
    the project's `.claude/settings.json`.
  - Adds a skill for the `query` commands, and a pointer to them in
    `AGENTS.md`.
- **Codex**: Codex has no language server support, so it gets the `query`
  commands as an MCP server (`clojure-lite-lsp mcp`):
  - For you, with `codex mcp add`.
  - For the project, in `.codex/config.toml` (Codex reads it once you trust
    the project).
  - Plus a section in `AGENTS.md`.

Either way the project is indexed, so the first question doesn't wait for indexing.
Run it again after upgrading; it replaces its own parts and keeps everything
else.

## Editors

- **VS Code**: [`vscode/`](vscode/README.md), an extension that adds this
  server to VS Code's Clojure support (`vscode/install`).
- **Zed**: [`zed/`](zed/README.md), an extension that provides the Clojure
  language and this server (it replaces the Clojure extension).
- **Others**: run `clojure-lite-lsp lsp` as the language server for Clojure
  files, over stdio.

## Command line

```sh
clojure-lite-lsp query                                # the query commands
clojure-lite-lsp query references app.core/foo       # every use of a var
clojure-lite-lsp query definition src/app/core.clj:12:5
clojure-lite-lsp query callers app.core/foo --json
clojure-lite-lsp index <dir>...                       # index projects and wait
clojure-lite-lsp status                               # the indexer, the index, its projects
clojure-lite-lsp gc                                   # collect garbage in the index now
clojure-lite-lsp stop                                 # stop the indexer
```

- **Targets**: a target is `ns/name` (a var), `ns` (a namespace) or
  `file:line:col` (1-based).
- **Output**: answers are `path:line:col: text` lines, paths relative to the
  project root.
- **Freshness**: `query` brings the project's index up to date first, which
  takes about a second on a big project; `--no-sync` skips it.

## Configuration

A project's classpath comes from `deps.edn` (aliases `:dev` and `:test`) or
`project.clj`, plus `bb.edn`'s; a folder with none indexes its `src` and
`test`. A `.clojure-lite-lsp.edn` at the project root can
change that:

```clojure
{:aliases [:dev :test :ee]      ; deps.edn aliases (or lein profiles)
 :extra-source-paths ["dev"]}   ; more source dirs
```

The index lives in your OS's cache directory (`~/Library/Caches/clojure-lite-lsp`
on macOS, `$XDG_CACHE_HOME/clojure-lite-lsp` or `~/.cache/clojure-lite-lsp` on
Linux; `CLOJURE_LITE_LSP_HOME` moves it), and the indexer's log is in its
`v<n>/daemon.log`. `clojure-lite-lsp status` shows where it is.

## License

[Apache License 2.0](LICENSE).
