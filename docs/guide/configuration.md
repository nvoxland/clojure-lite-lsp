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

The classpath is how libraries are found, so the project's build tool needs to
be installed: the Clojure CLI, `lein` or `bb`. Without it, only the project's
`src` and `test` are indexed, and library code can't be navigated to.

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

clojure-lite-lsp keeps its files in your OS's usual places: a cache directory
for everything it can rebuild, and a data directory for the one thing it
can't.

| | Cache directory | Data directory |
|---|---|---|
| macOS | `~/Library/Caches/clojure-lite-lsp` | `~/Library/Application Support/clojure-lite-lsp` |
| Linux | `$XDG_CACHE_HOME/clojure-lite-lsp` (default `~/.cache/clojure-lite-lsp`) | `$XDG_DATA_HOME/clojure-lite-lsp` (default `~/.local/share/clojure-lite-lsp`) |

In the cache directory:

| | |
|---|---|
| `v<n>/index.db` | The index, one per format version |
| `v<n>/daemon.log` | The indexer's log |
| `sources/` | Library sources, extracted when you navigate to them |
| `configs/` | Copies of the clj-kondo configs in use |

In the data directory:

| | |
|---|---|
| `claude-marketplace/` | The Claude Code plugin ([setup](../getting-started/agents/claude-code.md)) |

`clojure-lite-lsp status` shows where the index is.

`CLOJURE_LITE_LSP_HOME` puts all of it in one directory instead, e.g. to try
things out without touching your real index.
