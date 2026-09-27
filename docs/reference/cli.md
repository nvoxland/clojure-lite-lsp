# Command line

```text
clojure-lite-lsp <command>

  query <command> <arg>        look things up: definitions, references, callers, ...
  index [<project-dir>...]     index projects and wait; without dirs, run the indexer
  lsp                          the language server, for editors and agents (stdio)
  mcp                          the query commands as an MCP server, for agents (stdio)
  setup --agent <agent> [dir]  make a project ready for a coding agent: claude, codex
  gc                           collect garbage in the index now
  status                       the indexer, the index and its projects
  version
```

## `query`

Look things up in a project's index. See [Query commands](../guide/query.md).

## `index`

```sh
clojure-lite-lsp index <project-dir>...
```

Index projects and wait until they're done, printing progress. Useful from a
script that creates a worktree, so an editor or agent opened on it finds its
index ready.

Without directories, `clojure-lite-lsp index` runs the indexer itself in the
foreground. Editors and the other commands start it on their own; you rarely
need to.

## `lsp`

The language server, over stdio. See [Language server](lsp.md).

## `mcp`

The [query commands](../guide/query.md) as an MCP server over stdio, for agents
that speak MCP. The project is the server's working directory, or each call's
`project` argument. See [Codex](../agents/codex.md).

## `setup`

```sh
clojure-lite-lsp setup --agent <claude|codex>[,...] [dir] [--no-index]
```

Make a project (default: the current directory) ready for a coding agent. See
[Agents](../agents/index.md).

## `status`

The indexer (running or not), the index's size, and each project's indexed
files and jars, including how many jars it shares with other projects.

## `gc`

Collect garbage in the index now, through the running indexer (starting one if
needed). Prints what was dropped.

## Environment

| Variable | |
|---|---|
| `CLOJURE_LITE_LSP_HOME` | Where the index and its files live (default `~/.cache/clojure-lite-lsp`). |
| `CLOJURE_LITE_LSP_MARCH` | Build only: the CPU target for `bin/build-native` (default `native`). |
| `CLOJURE_LITE_LSP_BIN_DIR` | Build only: where `bin/install-server` links the binary (default `~/.local/bin`). |
