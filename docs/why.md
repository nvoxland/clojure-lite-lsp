# Why clojure-lite-lsp

Clojure language servers usually run one analysis per project, in a JVM, in
every editor window. That is fine for one checkout. It stops being fine when
you work the way agent-assisted development encourages: many worktrees of the
same project at once, each with an editor or an agent looking at it.

clojure-lite-lsp is built for that.

## Analysis is shared

Every analyzed file is stored once in a single index for the machine, keyed by
its content:

- A library jar is analyzed once, ever. Every project that uses it links to
  the same analysis.
- A new worktree of a project you already indexed is ready after analyzing
  only the files that differ from what's already there. For Metabase, a
  second worktree is ready in about 3 seconds and adds about 2.5 MB to the
  index.
- A branch with a slightly different clj-kondo config re-analyzes only the
  files that config change can affect.

## Memory stays small

The server an editor runs (`clojure-lite-lsp lsp`) is a native binary that only
reads the index: about 20 MB per editor, with Metabase's full classpath open.
Analysis happens in one background indexer for the whole machine, which exits
after ten idle minutes.

## It reads, and does that well

clojure-lite-lsp navigates: definitions, references, implementations, docs,
outlines, symbol search and call hierarchy. It doesn't edit code (no
diagnostics, formatting, completion or refactoring), so it has none of that
work to keep up with while you type.

## Agents are first-class

Agents mostly read code: where is this defined, who calls it. clojure-lite-lsp
gives them that through Claude Code's LSP tool, an MCP server for Codex, and a
`query` command any agent can run. `clojure-lite-lsp setup --agent …` sets a
project up in one step. See [Agents](agents/index.md).

## What it isn't

- **Not a full IDE backend.** Use a REPL-based tool (Calva, CIDER, Conjure) for
  evaluation, and clojure-lsp if you want diagnostics and refactorings.
- **Not a clojure-lsp fork.** It's an independent project with its own design,
  though it builds on clj-kondo's analysis just as clojure-lsp does.
