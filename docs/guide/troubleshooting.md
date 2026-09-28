# Troubleshooting

## Nothing happens in the editor

- Check `clojure-lite-lsp version` works in a terminal, and that its directory
  is on the `PATH` the editor sees. Otherwise, set the path in the editor's
  settings ([VS Code](../getting-started/editors/vscode.md#settings), [Zed](../getting-started/editors/zed.md#settings)).
- In Zed, the language must come from the clojure-lite-lsp extension: uninstall
  other extensions that define Clojure.

## Answers are missing or old

- `clojure-lite-lsp status` shows whether the project is indexed, and how much
  is still queued.
- The indexer's log is `v<n>/daemon.log` in the
  [cache directory](configuration.md#where-things-live). A classpath
  that can't be computed (a broken `deps.edn`, `clojure` not on the indexer's
  `PATH`) shows in `clojure-lite-lsp status`, in the editor once, and in the
  log; the last working classpath is used meanwhile.
- `clojure-lite-lsp index <project>` re-syncs a project and waits for it.

## Duplicate results

Another Clojure language server is running too: Calva's clojure-lsp, or Zed's
Clojure extension. Turn one off ([VS Code](../getting-started/editors/vscode.md)).

## Starting over

The index is only a cache. Close editors, stop the indexer, then delete the
[cache directory](configuration.md#where-things-live):

```sh
clojure-lite-lsp stop
rm -rf ~/Library/Caches/clojure-lite-lsp   # macOS
rm -rf ~/.cache/clojure-lite-lsp           # Linux
```

The next editor or query rebuilds the index.
