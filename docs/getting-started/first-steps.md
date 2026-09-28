# First steps

## Index a project

Editors and agents index projects on their own, but you can do it up front:

```sh
clojure-lite-lsp index ~/src/my-project
```

This starts the background indexer if it isn't running, and waits until the
project is indexed. The first project takes longest, because its libraries
are analyzed too (Metabase, with about 5,000 source files and 480 jars: about
35 seconds). Libraries are shared, so later projects that use them are faster,
and another worktree of the same project takes seconds.

## Ask it something

From anywhere inside the project:

```sh
clojure-lite-lsp query definition clojure.string/join
clojure-lite-lsp query references my.app.core/handler
clojure-lite-lsp query callers my.app.core/handler
clojure-lite-lsp query definition src/my/app/core.clj:42:10
```

Answers are one `path:line:col: text` line per result. `clojure-lite-lsp query`
on its own lists every command; see [Query commands](../guide/query.md).

## See what's indexed

```sh
clojure-lite-lsp status
```

```text
Daemon:  pid 40510, version 0.1.0-SNAPSHOT, heartbeat 4 s ago
Index:   221 MB, 5505 analyzed files, 479 jars

/Users/me/src/metabase
  files: 3312/3312 indexed
  jars: 479 (0 shared with other projects)
```

## Hook it into your editor or agent

- [VS Code](../editors/vscode.md) · [Zed](../editors/zed.md) · [others](../editors/other.md)
- [Claude Code](../agents/claude-code.md) · [Codex](../agents/codex.md)
