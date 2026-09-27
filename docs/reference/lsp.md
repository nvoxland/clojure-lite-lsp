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
| `textDocument/documentHighlight` | The occurrences of what's under the cursor in the file: definitions and bindings as writes, uses as reads |
| `textDocument/prepareRename`, `rename` | Locals only: every place the local is written, all in one file (both languages of a `.cljc` file). Anything else is refused, so editors offer rename only on locals; so is a local whose code changed since the last save. |
| `textDocument/signatureHelp` | The arglists of the call being typed, with the current argument. A call the index hasn't seen yet is resolved by name: through the file's aliases, its own namespace, then `clojure.core` |

Documents sync incrementally; `didSave` and `workspace/didChangeWatchedFiles`
(registered dynamically) queue re-indexing. Progress is reported with
`$/progress` while the index builds, to clients that support it.

Answers keep to the language asked about: in a `.cljs` file, a var resolves to
its ClojureScript definition, and a clj macro counts for cljs callers through
`:require-macros`. Likewise a namespace defined only in Clojure (a macro
namespace) is referenced from the ClojureScript files that require it.

## Not supported

Diagnostics, completion, formatting, code actions, renaming vars and
namespaces, and semantic tokens. Apart from renaming locals, clojure-lite-lsp
doesn't edit code: edits across files, made from an index, could be wrong
whenever the index is behind.

## Initialization options

| Option | Default | |
|---|---|---|
| `dependency-scheme` | `"file"` | How library sources are returned: `"file"` (extracted, read-only files), or `"jar"` / `"zipfile"` URIs for editors that open those. |
| `waitForIndex` | `false` | Hold requests until the project's index is up to date, instead of answering from what's indexed so far. For agents. |

## Custom request

`clojure-lite-lsp/status` returns the queue's state, e.g. `{"pending": 0}`.
