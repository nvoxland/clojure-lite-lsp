# Query commands

`clojure-lite-lsp query` looks things up in a project's index from the command
line. It's what agents use (directly, through Claude Code's skill, or as MCP
tools), and it's handy in a terminal or a script.

```sh
clojure-lite-lsp query <command> <argument> [--json] [--no-sync] [--limit <n>] [--project <dir>]
```

## Targets

Most commands take a **target**:

- `ns/name`: a var, e.g. `clojure.string/join` or `my.app.core/handler`.
- `ns`: a namespace, e.g. `my.app.core`.
- `:kw` or `:ns/kw`: a keyword.
- `file:line:col`: whatever is at that position, 1-based as editors and
  compilers print them, e.g. `src/my/app/core.clj:42:10`. A relative file is
  looked for from the current directory, then the project root, so paths from
  results work as they are. Positions in extracted library sources work too.

## Commands

| Command | Argument | |
|---|---|---|
| `definition` | target | Where a var or namespace is defined. |
| `references` | target | Every use of a var, namespace or keyword. |
| `implementations` | target | Implementations of a protocol or protocol method, and a multimethod's methods. |
| `doc` | target | A var's or namespace's arglists and docstring. |
| `callers` | target | Where a function is called, and from which function. |
| `callees` | target | What a function calls, and where. |
| `symbols` | text | Definitions whose name contains the text, exact names first, then the project's own. |
| `outline` | file | The namespaces and vars a file defines, in order. |

`clojure-lite-lsp query` on its own prints this list.

## Output

One line per result, `path:line:col:` then what's there. Paths are relative to
the project root; library sources are extracted, read-only files under
`~/.cache/clojure-lite-lsp/sources/`.

```text
$ clojure-lite-lsp query references app.a/greet
src/app/a.clj:6:18: (defn twice [x] (greet x) (greet x))
src/app/a.clj:6:28: (defn twice [x] (greet x) (greet x))
src/app/b.clj:2:16: (defn main [] (a/greet "you"))

$ clojure-lite-lsp query callers app.a/greet
src/app/a.clj:6:18: app.a/twice calls it
src/app/b.clj:2:16: app.b/main calls it

$ clojure-lite-lsp query doc app.a/greet
app.a/greet
[who]
Says hello.
src/app/a.clj:2:7
```

`--json` prints `{"results": [...], "total": n}`: each result has `path`,
`line`, `column`, `end-line` and `end-column`, plus the command's own fields
(`symbol`, `caller`, `callee`, `arglists`, `doc`, `kind`).

At most `--limit` results are shown (default 200), followed by how many more
there are. When nothing matches, it prints `No results.`

## Freshness

The project is the one containing the current directory (the nearest directory
with a `deps.edn`, `project.clj`, `bb.edn` or `.clojure-lite-lsp.edn`), or the
one containing `--project <dir>`. Outside any project, `query` says so.

Before answering, `query` brings that project's index up to date, so recent
edits are included: about a second on a big project when little changed. The
first query on a project indexes it, which takes longer. `--no-sync` answers
from the index as it is, in a few milliseconds.

## Exit codes

| Code | |
|---|---|
| `0` | Answered (possibly `No results.`). |
| `1` | Couldn't answer: not in a Clojure project, not indexed yet (with `--no-sync`), no index, or a failure (the indexer didn't start, a missing `--project` directory). |
| `2` | A usage error: an unknown command or option, or a missing argument. |
