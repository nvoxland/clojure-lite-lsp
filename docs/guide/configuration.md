# Configuration

Most projects need none: clojure-lite-lsp reads the classpath from the project's
build file, and the clj-kondo configuration from its `.clj-kondo` directory.

## The classpath

| Build file | Classpath from |
|---|---|
| `deps.edn` | `clojure -Spath`, with the `:dev` and `:test` aliases the project defines |
| `project.clj` | `lein with-profile +dev,+test classpath` |
| `bb.edn` | `bb`'s classpath; beside `deps.edn` or `project.clj`, added to theirs |
| none | `src` and `test`, where they exist |

The classpath is recomputed only when a build file changes: the project's,
those of its `:local/root` dependencies, or your `~/.clojure/deps.edn` (or
`$CLJ_CONFIG/deps.edn`) and `~/.lein/profiles.clj`. If computing it fails (a build file mid-edit, the
tool offline), the last classpath that worked is used; with none yet, the
`:paths` of `deps.edn` (or `src` and `test`). `clojure-lite-lsp status` and the
editor say so.

Files under the project root but outside its source directories, like
`build.clj`, are indexed once opened. Java classes resolve to the JDK's
`src.zip`, found under `JAVA_HOME`, the running Java, or `/usr/libexec/java_home`.

## `.clojure-lite-lsp.edn`

At the project root:

```clojure
{:aliases [:dev :test :ee]      ; deps.edn aliases, or lein profiles
 :extra-source-paths ["dev"]}   ; more directories to index as the project's own
```

A folder without a build file indexes its `src` and `test`; `:extra-source-paths`
adds others.

## clj-kondo configuration

The project's `.clj-kondo/config.edn` (with its hooks, `:lint-as` and so on)
applies to its own files, together with the configs its dependencies export.
A library is analyzed with its own exported config only.

Your personal `~/.config/clj-kondo` isn't used: indexes are shared between
projects, so they depend only on what's in each project.

Edits to `.clj-kondo/` (config or hooks) take effect on the next save: the
project's files are re-analyzed as needed.

## Where things live

| | |
|---|---|
| `~/.cache/clojure-lite-lsp/v<n>/index.db` | The index, one per format version |
| `~/.cache/clojure-lite-lsp/v<n>/daemon.log` | The indexer's log |
| `~/.cache/clojure-lite-lsp/sources/` | Library sources, extracted when you navigate to them |
| `~/.cache/clojure-lite-lsp/configs/` | Copies of the clj-kondo configs in use |
| `~/.cache/clojure-lite-lsp/claude-marketplace/` | The Claude Code plugin ([setup](../agents/claude-code.md)) |

`CLOJURE_LITE_LSP_HOME` moves all of it, e.g. to try things out without
touching your real index.
