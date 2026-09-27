# clojure-lite-lsp

A low-memory, read-only Clojure language server, for editors and coding agents.

- **Navigation**: definitions, references, implementations, hover docs, file
  outlines, symbol search and call hierarchy (callers and callees).
- **While you type**: occurrence highlighting, argument hints, and renaming
  locals.
- **One shared index**: analysis comes from
  [clj-kondo](https://github.com/clj-kondo/clj-kondo) and is stored in one
  SQLite index for every project on the machine. A library is analyzed once;
  another worktree of a project is ready after indexing only the files that
  differ.
- **Light**: each editor's server process is about 20 MB, and indexing
  happens in a shared background process that exits when idle.

Apart from renaming locals, it doesn't edit code: no diagnostics, formatting,
completion or project-wide refactoring.

**Documentation: <https://nvoxland.github.io/clojure-lite-lsp/>** (sources in
[`docs/`](docs/); preview with `poetry install --with docs && poetry run mkdocs serve`).

## Install

Build the native binary and put it on your PATH (needs
[GraalVM](https://www.graalvm.org/), as `JAVA_HOME` or with `native-image` on
PATH, and the [Clojure CLI](https://clojure.org/guides/install_clojure)):

```sh
bin/install-server            # builds target/clojure-lite-lsp, links ~/.local/bin/clojure-lite-lsp
bin/install-server --rebuild  # after pulling changes
```

## Editors

- **Zed**: [`zed/`](zed/README.md), an extension that provides the Clojure
  language and this server (it replaces the Clojure extension).
- **VS Code**: [`vscode/`](vscode/README.md), an extension that adds this
  server to VS Code's Clojure support (`vscode/install`).
- **Others**: run `clojure-lite-lsp lsp` as the language server for Clojure
  files, over stdio.

## Coding agents

```sh
clojure-lite-lsp setup --agent claude   # or codex, or claude,codex; in the project's directory
```

- **Claude Code**:
  - Installs a plugin that gives Claude's LSP tool this server, recorded in
    the project's `.claude/settings.json`.
  - Adds a skill for the `query` commands, and a pointer to them in
    `CLAUDE.md`.
- **Codex**: Codex has no language server support, so it gets the `query`
  commands as an MCP server (`clojure-lite-lsp mcp`):
  - For you, with `codex mcp add`.
  - For the project, in `.codex/config.toml` (Codex reads it once you trust
    the project).
  - Plus a section in `AGENTS.md`.

Either way the project is indexed, so the first question is answered at once.
Run it again after upgrading; it replaces its own parts and keeps everything
else.

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

A project's classpath comes from `deps.edn` (aliases `:dev` and `:test`),
`project.clj` or `bb.edn`. A `.clojure-lite-lsp.edn` at the project root can
change that:

```clojure
{:aliases [:dev :test :ee]      ; deps.edn aliases (or lein profiles)
 :extra-source-paths ["dev"]}   ; more source dirs
```

The index lives in `~/.cache/clojure-lite-lsp` (`CLOJURE_LITE_LSP_HOME` moves
it), and the indexer's log is in its `v<n>/daemon.log`.

## License

[Apache License 2.0](LICENSE). The Zed extension includes files from Zed's
Clojure extension, also Apache 2.0: see [NOTICE](NOTICE).
