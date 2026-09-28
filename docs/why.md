# Why clojure-lite-lsp

## Small per project and worktree

Working with coding agents means more codebases open at once: several
projects, several worktrees of each, with an editor window or an agent session
in every one. What a language server costs for one project adds up quickly
across a dozen.

clojure-lite-lsp keeps the cost of each one low:

- Each editor's server uses about 20 MB of memory, whatever the project's size.
- Indexing happens in one background process for the whole machine, which
  exits when it's been idle for a while.
- A library is indexed once, and every project that uses it shares that.
- A new worktree only indexes the files that differ from what's already
  indexed. For a large project like Metabase, a worktree of a branch you
  already have takes a few seconds.

## Reading, not writing

It stays that small by doing less. With an agent making the edits, less of
your time goes into typing code and more into reading it: following what a
change touches, checking who calls a function, reviewing a diff. Much of what
language servers do (completion, diagnostics as you type, refactorings) is
there to help you write code by hand, and keeping the analysis ready for it
is much of what makes them large.

So clojure-lite-lsp implements the reading side of the Language Server
Protocol:

- go to definition and declaration
- find references, including through aliases and refers
- implementations of protocols and multimethods
- hover: arglists and docstrings
- file outlines and symbol search
- call hierarchy: callers and callees
- occurrence highlighting and argument hints

It leaves out the writing side: completion, diagnostics, formatting, rename,
refactorings and code actions. Editors see that the server doesn't offer these
and don't show them. See [Language server](reference/lsp.md) for the details.

## The same answers for agents

Agents ask the same questions you do: where is this defined, what calls it.
Claude Code gets the answers through its LSP tool, Codex through an MCP server,
and any agent through the `clojure-lite-lsp query` command.
`clojure-lite-lsp setup --agent …` configures a project for them. See
[Agents](agents/index.md).

## What it doesn't do

It doesn't evaluate code: keep your REPL tooling (Calva, CIDER, Conjure) for
that.
