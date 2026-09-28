# Zed extension: clojure-lite-lsp

Clojure language support for Zed, with `clojure-lite-lsp lsp` as its language
server. It replaces Zed's **Clojure** extension: it has the same tree-sitter
grammar and queries (syntax highlighting, brackets, indentation, outline), and
this server instead of clojure-lsp.

## Install

1. [Install the binary](https://nvoxland.github.io/clojure-lite-lsp/getting-started/install/),
   so `clojure-lite-lsp` is on your PATH (or [build it](../DEV.md)).

2. In Zed, uninstall the **Clojure** extension (and any other extension that
   defines the `Clojure` language, like **Clojure (nREPL LSP)**): they would
   define the same language twice.

3. Run **zed: install dev extension** and select this `zed/` directory. Zed
   builds it, which needs Rust installed through rustup, and fetches the
   grammar.

4. Open a `.clj` file: the status bar shows Clojure, and `clojure-lite-lsp`
   is in the language servers list.

## Settings

The extension looks for the server in this order:

1. `lsp.clojure-lite-lsp.binary.path` in Zed settings, if set;
2. `clojure-lite-lsp` on the worktree's PATH;
3. otherwise the server fails to start, and the error says how to configure it.

It runs `clojure-lite-lsp lsp` unless `binary.arguments` says otherwise; `binary.env` is
passed through. For example, to keep a separate index while trying it out:

```json
{
  "lsp": {
    "clojure-lite-lsp": {
      "binary": {
        "path": "/path/to/clojure-lite-lsp/target/clojure-lite-lsp",
        "env": { "CLOJURE_LITE_LSP_HOME": "/tmp/clojure-lite-lsp-try" }
      }
    }
  }
}
```

## Using it

- The first time a project opens, clojure-lite-lsp starts a background indexer (`clojure-lite-lsp
  index`, the same binary) and Zed shows indexing progress. The first index
  of a big project takes a while (about 35 s for 5,000 source files); a
  second worktree of it takes a few seconds, and reopening needs no indexing.
- Go to definition into a library opens its source as a read-only file under
  `sources/` in clojure-lite-lsp's cache directory, fully navigable once open.
- `clojure-lite-lsp status` in a terminal shows the indexer, the index, and each project's
  files and jars, including how many jars are shared with other projects.
- **dev: open language server logs** shows the server's stderr. The indexer
  logs to `v<n>/daemon.log` in its cache directory
  ([where that is](https://nvoxland.github.io/clojure-lite-lsp/guide/configuration/#where-things-live)).
- A project's classpath comes from `deps.edn` (aliases `:dev` and `:test` by
  default), `project.clj` or `bb.edn`. A `.clojure-lite-lsp.edn` at the project root can
  change that:

  ```clojure
  {:aliases [:dev :test :ee]      ; deps.edn aliases (or lein profiles)
   :extra-source-paths ["dev"]}   ; indexed as project sources
  ```

## Development

```sh
cargo test                                   # launch-command resolution tests
cargo build --release --target wasm32-wasip2 # what Zed builds on install
```

## License

The files in `languages/clojure/` are copied from Zed's Clojure extension
(Apache License 2.0, see `LICENSE-zed-clojure` and `NOTICE`); the grammar is
[tree-sitter-clojure](https://github.com/prcastro/tree-sitter-clojure).
