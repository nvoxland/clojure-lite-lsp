# Command line

```text
clojure-lite-lsp <command>

  query <command> <arg>        look things up: definitions, references, callers, ...
  index [<project-dir>...]     index projects and wait; without dirs, run the indexer
  lsp                          the language server, for agents and editors (stdio)
  mcp                          the query commands as an MCP server, for agents (stdio)
  setup --agent <agent> [dir]  make a project ready for a coding agent: claude, codex
  gc                           collect garbage in the index now
  stop                         stop the indexer (it restarts when needed)
  status                       the indexer, the index and its projects
  version
```

There's nothing to start or set up first: whatever needs the index (a query,
an agent, an editor) starts the background indexer, and a project is indexed
the first time it's asked about. The first project takes longest, because its
libraries are indexed too: a project with about 5,000 source files and 480
jars takes about 35 seconds. Libraries are shared, so later projects that use
them are faster, and another worktree of the same project takes seconds.

## `query`

Look things up in a project's index. See [Query commands](query.md).

## `index`

```sh
clojure-lite-lsp index <project-dir>...
```

Index projects and wait until they're done, printing progress. Useful from a
script that creates a worktree, so an agent or editor opened on it finds its
index ready.

Without directories, `clojure-lite-lsp index` runs the indexer itself in the
foreground. Editors and the other commands start it on their own; you rarely
need to.

## `lsp`

The language server, over stdio. See [Language server](lsp.md).

## `mcp`

The [query commands](query.md) as an MCP server over stdio, for agents
that speak MCP. The project is the one containing the server's working
directory, or each call's `project` argument. See [MCP](mcp.md).

## `setup`

```sh
clojure-lite-lsp setup --agent <claude|codex>[,...] [dir] [--no-index]
```

Make a project (default: the current directory) ready for a coding agent. See
[Agents](../getting-started/agents/index.md).

## `stop`

Stop the indexer once it finishes its current batch. Editors and queries
start it again when they need it.

## `status`

The indexer (running or not), the index's size and where it is, and each
project's indexed files and jars, including how many jars it shares with
other projects:

```text
Daemon:  pid 40510, version 0.2.0, heartbeat 4 s ago
Index:   221 MB, 5505 analyzed files, 479 jars
         /Users/me/Library/Caches/clojure-lite-lsp/v7/index.db

/Users/me/src/my-app
  files: 3312/3312 indexed
  jars: 479 (0 shared with other projects)
```

## `gc`

Collect garbage in the index now, through the running indexer (starting one if
needed). Prints what was dropped.

## Environment

| Variable | |
|---|---|
| `CLOJURE_LITE_LSP_HOME` | One directory for all of clojure-lite-lsp's files, instead of your OS's cache and data directories ([Where things live](configuration.md#where-things-live)). |
| `CLJ_CONFIG` | Where the Clojure CLI's user `deps.edn` is, as for `clojure` itself. |
