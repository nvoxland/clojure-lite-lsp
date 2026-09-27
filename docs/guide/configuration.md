# Configuration

Most projects need none: clojure-lite-lsp reads the classpath from the project's
build file, and the clj-kondo configuration from its `.clj-kondo` directory.

## The classpath

| Build file | Classpath from |
|---|---|
| `deps.edn` | `clojure -Spath`, with the `:dev` and `:test` aliases the project defines |
| `project.clj` | `lein with-profile +dev,+test classpath` |
| `bb.edn` | `bb`'s classpath |

The classpath is recomputed only when a build file changes: the project's,
those of its `:local/root` dependencies, or your `~/.clojure/deps.edn` (or
`$CLJ_CONFIG/deps.edn`) and `~/.lein/profiles.clj`. If computing it fails (a build file mid-edit, the
tool offline), the last classpath that worked is used.

## `.clojure-lite-lsp.edn`

At the project root:

```clojure
{:aliases [:dev :test :ee]      ; deps.edn aliases, or lein profiles
 :extra-source-paths ["dev"]}   ; more directories to index as the project's own
```

A project without a build file can use `:extra-source-paths` alone.

## clj-kondo configuration

The project's `.clj-kondo/config.edn` (with its hooks, `:lint-as` and so on)
applies to its own files, together with the configs its dependencies export.
Each library is analyzed with its own exported config only, which is what
lets its analysis be shared by every project that uses it.

Your personal `~/.config/clj-kondo` is deliberately not used: analysis is
shared, so it has to depend only on the project.

## Where things live

| | |
|---|---|
| `~/.cache/clojure-lite-lsp/v<n>/index.db` | The index (SQLite), one per format version |
| `~/.cache/clojure-lite-lsp/v<n>/daemon.log` | The indexer's log |
| `~/.cache/clojure-lite-lsp/sources/` | Library sources, extracted when you navigate to them |
| `~/.cache/clojure-lite-lsp/configs/` | Private copies of clj-kondo configs |
| `~/.cache/clojure-lite-lsp/claude-marketplace/` | The Claude Code plugin ([setup](../agents/claude-code.md)) |

`CLOJURE_LITE_LSP_HOME` moves all of it, e.g. to try things out without
touching your real index.
