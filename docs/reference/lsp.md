# Language server

`clojure-lite-lsp lsp` speaks the Language Server Protocol over stdio.

## Supported

| Request | |
|---|---|
| `textDocument/definition` | Vars, namespaces, keywords (`re-frame`-style registrations), locals, Java classes (to their `.java` source when available) |
| `textDocument/declaration` | Where the file brings a var (or an `::al/kw` keyword) in: the alias it's written through, its `:refer` entry, or its namespace's require. Otherwise the definition |
| `textDocument/references` | Uses of vars, namespaces, keywords, locals and Java classes. On an alias (`:as al`), the names written through it in its file |
| `textDocument/implementation` | Protocol and protocol-method implementations, multimethod methods |
| `textDocument/hover` | Arglists and docstring |
| `textDocument/documentSymbol` | The file's namespaces and definitions |
| `workspace/symbol` | Definitions by name, exact matches first |
| `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `outgoingCalls` | Callers and callees |
| `textDocument/documentHighlight` | The occurrences of what's under the cursor in the file: definitions and bindings as writes, uses as reads |
| `textDocument/signatureHelp` | The arglists of the call being typed, with the current argument. A call the index hasn't seen yet is resolved by name: through the file's aliases, its own namespace, then `clojure.core` |

Documents sync incrementally; `didSave` and `workspace/didChangeWatchedFiles`
(registered dynamically) queue re-indexing. Progress is reported with
`$/progress` while the index builds, to clients that support it.

Answers keep to the language asked about: in a `.cljs` file, a var resolves to
its ClojureScript definition, and a clj macro counts for cljs callers through
`:require-macros`. Likewise a namespace defined only in Clojure (a macro
namespace) is referenced from the ClojureScript files that require it.

## Not supported

Diagnostics, completion, formatting, code actions, rename and semantic
tokens. clojure-lite-lsp only reads code, and doesn't advertise these, so
editors leave them to other tools (or don't offer them).

## Initialization options

| Option | Default | |
|---|---|---|
| `dependency-scheme` | `"file"` | How library sources are returned: `"file"` (extracted, read-only files), or `"jar"` / `"zipfile"` URIs for editors that open those. |
| `waitForIndex` | `false` | Hold requests until the project's index is up to date, instead of answering from what's indexed so far. For agents. |

## Custom request

`clojure-lite-lsp/status` returns the queue's state, e.g. `{"pending": 0}`.
