# Zed extension: Clojure (SQLite LSP)

Registers `csl lsp` as a language server for Zed's `Clojure` language. It
registers only the server: the language itself (grammar, highlighting) comes
from another extension, Zed's own **Clojure** extension or **Clojure (nREPL
LSP)**.

## Install

1. Build the server and put `csl` on your PATH:

   ```sh
   zed/install-server            # builds target/csl if needed, links ~/.local/bin/csl
   zed/install-server --rebuild  # after pulling changes
   ```

   Building needs GraalVM (`JAVA_HOME` pointing at it, or `native-image` on
   PATH).

2. In Zed, run **zed: install dev extension** and select this `zed/`
   directory. Zed builds it, which needs Rust installed through rustup.

3. Choose which Clojure language servers run (Zed settings). To use only
   this one:

   ```json
   {
     "languages": {
       "Clojure": {
         "language_servers": ["clojure-sqlite-lsp", "!clojure-lsp", "!clojure-nrepl-lsp", "..."]
       }
     }
   }
   ```

   Both servers can also run side by side, but then Zed shows each result
   twice. `"..."` keeps any other servers registered for Clojure.

## Settings

The extension looks for the server in this order:

1. `lsp.clojure-sqlite-lsp.binary.path` in Zed settings, if set;
2. `csl` on the worktree's PATH;
3. otherwise the server fails to start, and the error says how to configure it.

It runs `csl lsp` unless `binary.arguments` says otherwise; `binary.env` is
passed through. For example, to keep a separate index while trying it out:

```json
{
  "lsp": {
    "clojure-sqlite-lsp": {
      "binary": {
        "path": "/path/to/clojure-sqlite-lsp/target/csl",
        "env": { "CSL_HOME": "/tmp/csl-try" }
      }
    }
  }
}
```

## Using it

- The first time a project opens, csl starts a background indexer (`csl
  index`, the same binary) and Zed shows indexing progress. The first index
  of a big project takes a while (Metabase: about 35 s); a second worktree
  of it takes a few seconds, and reopening is instant.
- Go to definition into a library opens its source as a read-only file under
  `~/.cache/clojure-sqlite-lsp/sources/`, fully navigable once open.
- `csl status` in a terminal shows the indexer, the index, and each project's
  files and jars, including how many jars are shared with other projects.
- **dev: open language server logs** shows the server's stderr. The indexer
  logs to `~/.cache/clojure-sqlite-lsp/daemon.log`.
- A project's classpath comes from `deps.edn` (aliases `:dev` and `:test` by
  default), `project.clj` or `bb.edn`. A `.csl.edn` at the project root can
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
