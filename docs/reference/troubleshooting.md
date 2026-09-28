# Troubleshooting

## Nothing happens in the editor

- Check `clojure-lite-lsp version` works in a terminal, and that its directory
  is on the `PATH` the editor sees. Otherwise, set the path in the editor's
  settings ([Zed](../editors/zed.md#settings), [VS Code](../editors/vscode.md#settings)).
- In Zed, the language must come from the clojure-lite-lsp extension: uninstall
  other extensions that define Clojure.

## Answers are missing or old

- `clojure-lite-lsp status` shows whether the project is indexed, and how much
  is still queued.
- The indexer's log is `~/.cache/clojure-lite-lsp/v<n>/daemon.log`. A classpath
  that can't be computed (a broken `deps.edn`, `clojure` not on the indexer's
  `PATH`) shows in `clojure-lite-lsp status`, in the editor once, and in the
  log; the last working classpath is used meanwhile.
- `clojure-lite-lsp index <project>` re-syncs a project and waits for it.

## Duplicate results

Another Clojure language server is running too: Calva's clojure-lsp, or Zed's
Clojure extension. Turn one off ([VS Code](../editors/vscode.md)).

## Starting over

The index is only a cache. Close editors, stop the indexer, then delete it:

```sh
clojure-lite-lsp stop
rm -rf ~/.cache/clojure-lite-lsp
```

The next editor or query rebuilds the index. The same directory holds the
Claude Code plugin's marketplace, so for projects set up for Claude Code, run
`clojure-lite-lsp setup --agent claude` in them again.
