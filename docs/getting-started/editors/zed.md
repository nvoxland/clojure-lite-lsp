# Zed

The Zed extension in [`zed/`](https://github.com/nvoxland/clojure-lite-lsp/tree/main/zed)
provides the Clojure language (the same tree-sitter grammar and queries as Zed's
Clojure extension) with clojure-lite-lsp as its language server. It replaces
the Clojure extension.

## Install

1. [Install the binary](../install.md), so `clojure-lite-lsp` is on your `PATH`.
2. In Zed, uninstall the **Clojure** extension, and any other extension that
   defines the `Clojure` language: two would define the same language.
3. Run **zed: install dev extension** and select the repository's `zed/`
   directory. Zed builds it, which needs Rust installed through rustup.
4. Open a `.clj` file. The status bar shows Clojure, and `clojure-lite-lsp`
   appears in the language servers list.

## Settings

The extension finds the server through `lsp.clojure-lite-lsp.binary.path` if
set, else `clojure-lite-lsp` on the worktree's `PATH`. It passes your shell's
environment, so the indexer finds `clojure`, `lein` and `bb` even when Zed was
started from the Dock.

```json
{
  "lsp": {
    "clojure-lite-lsp": {
      "binary": {
        "path": "/path/to/clojure-lite-lsp",
        "env": { "CLOJURE_LITE_LSP_HOME": "/tmp/clojure-lite-lsp-try" }
      }
    }
  }
}
```

## Using it

- Indexing progress shows in the status bar.
- Go to definition into a library opens its source as a read-only file under
  `sources/` in the [cache directory](../../guide/configuration.md#where-things-live),
  fully navigable once open.
- **dev: open language server logs** shows the server's log.
