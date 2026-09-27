# Codex

Codex doesn't support language servers yet; its extension point is MCP. So
clojure-lite-lsp offers its [query commands](../guide/query.md) as MCP tools:
`clojure-lite-lsp mcp`.

## Set up

```sh
clojure-lite-lsp setup --agent codex
```

In the project's directory. It:

1. Registers the MCP server for you (`codex mcp add clojure-lite-lsp --
   clojure-lite-lsp mcp`), so it works in every project: the server finds the
   project from the directory Codex runs in.
2. Adds it to the project's `.codex/config.toml`, for teammates. Codex reads a
   project's config only once you trust the project.
3. Adds a section to `AGENTS.md` describing the tools and the `query` commands.
4. Indexes the project.

## The tools

`definition`, `references`, `implementations`, `doc`, `callers`, `callees`,
`symbols` and `outline`. Each takes a `target` (`ns/name`, `ns`, `:kw`, or
`file:line:col`, 1-based), an optional `project` (a directory in the project;
default: where Codex runs) and `limit` (default 100), and answers as
`clojure-lite-lsp query` does: one `path:line:col: text` line per result.

A call waits for the project's index at most 40 seconds (Codex gives a tool
60). If indexing takes longer (a big project's first time), it answers from
what's indexed so far and says indexing goes on.

```text
mcp: clojure-lite-lsp/callers started
mcp: clojure-lite-lsp/callers (completed)
src/app/b.clj:2:16: app.b/main calls it
```

## By hand

Without `setup`, add it to `~/.codex/config.toml` (or a trusted project's
`.codex/config.toml`):

```toml
[mcp_servers.clojure-lite-lsp]
command = "clojure-lite-lsp"
args = ["mcp"]
```

Any other MCP client can use the same command.
