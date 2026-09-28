# Codex

Codex doesn't support language servers yet; its extension point is MCP. So
clojure-lite-lsp offers its [query commands](../../guide/query.md) as MCP tools:
[`clojure-lite-lsp mcp`](../../guide/mcp.md).

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
3. Adds a short section to `AGENTS.md` pointing Codex at the tools instead of grep.
4. Indexes the project.

## The tools

`definition`, `references`, `callers` and the other
[query commands](../../guide/query.md), each taking a `target` like
`ns/name`. See [MCP](../../guide/mcp.md) for their arguments and how they
wait for indexing.

## By hand

Without `setup`, add it to `~/.codex/config.toml` (or a trusted project's
`.codex/config.toml`):

```toml
[mcp_servers.clojure-lite-lsp]
command = "clojure-lite-lsp"
args = ["mcp"]
```

Any other MCP client can use the same command ([MCP](../../guide/mcp.md)).
