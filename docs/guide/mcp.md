# MCP

`clojure-lite-lsp mcp` offers the [query commands](query.md) as tools over
MCP (stdio), for agents that don't use language servers, like
[Codex](../getting-started/agents/codex.md).

## Tools

`definition`, `references`, `implementations`, `doc`, `callers`, `callees`,
`symbols` and `outline`. Each takes:

| Argument | |
|---|---|
| `target` | `ns/name`, `ns`, `:kw`, or `file:line:col` (1-based) |
| `project` | A directory in the project. Default: the project containing the server's working directory |
| `limit` | At most this many results (default 100) |

and answers as `clojure-lite-lsp query` does: one `path:line:col: text` line
per result, paths relative to the project root.

```text
mcp: clojure-lite-lsp/callers started
mcp: clojure-lite-lsp/callers (completed)
src/app/b.clj:2:16: app.b/main calls it
```

## Waiting for the index

A call brings the project's index up to date first, waiting at most 40
seconds (under the usual 60-second tool timeouts). If indexing takes longer,
as a big project's first time can, it answers from what's indexed so far and
says indexing goes on.

## Configuring a client

Any MCP client runs it the same way: the command `clojure-lite-lsp` with the
argument `mcp`. For example, in Codex's `~/.codex/config.toml`:

```toml
[mcp_servers.clojure-lite-lsp]
command = "clojure-lite-lsp"
args = ["mcp"]
```

One server serves every project: started from a project's directory, that's
its default project, and each call can name another.
