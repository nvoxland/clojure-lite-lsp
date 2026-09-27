# Language server

`clojure-lite-lsp lsp` speaks the Language Server Protocol over stdio.

## Supported

| Request | |
|---|---|
| `textDocument/definition`, `declaration` | Vars, namespaces, keywords (`re-frame`-style registrations), locals, Java classes (to their `.java` source when available) |
| `textDocument/references` | Uses of vars, namespaces, keywords, locals and Java classes |
| `textDocument/implementation` | Protocol and protocol-method implementations, multimethod methods |
| `textDocument/hover` | Arglists and docstring |
| `textDocument/documentSymbol` | The file's namespaces and definitions |
| `workspace/symbol` | Definitions by name, exact matches first |
| `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `outgoingCalls` | Callers and callees |

Documents sync incrementally; `didSave` and `workspace/didChangeWatchedFiles`
(registered dynamically) queue re-indexing. Progress is reported with
`$/progress` while the index builds.

Answers keep to the language asked about: in a `.cljs` file, a var resolves to
its ClojureScript definition, and a clj macro counts for cljs callers through
`:require-macros`.

## Not supported

Diagnostics, completion, formatting, code actions, renaming and semantic
tokens. clojure-lite-lsp is read-only by design.

## Initialization options

| Option | Default | |
|---|---|---|
| `dependency-scheme` | `"file"` | How library sources are returned: `"file"` (extracted, read-only files), or `"jar"` / `"zipfile"` URIs for editors that open those. |
| `waitForIndex` | `false` | Hold requests until the project's index is up to date, instead of answering from what's indexed so far. For agents. |

## Custom request

`clojure-lite-lsp/status` returns the queue's state, e.g. `{"pending": 0}`.
