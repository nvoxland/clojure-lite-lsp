# Agents

Coding agents read far more code than they write: where is this defined, who
calls it, what implements it. clojure-lite-lsp answers those questions with
Clojure's semantics (namespaces, aliases, refers, macros), where grep matches
strings, comments and the wrong `foo` in another namespace.

It suits how agents work, too:

- **Many worktrees in parallel**: each one costs a few seconds to index and
  a few MB of index, and the servers are small.
- **Whole answers**: a query brings the project's index up to date first, so
  what the agent just edited is included.

## Three ways in

| | How the agent uses it | Set up by |
|---|---|---|
| **Claude Code** | Its built-in LSP tool (definition, references, hover, call hierarchy, …), plus the `query` commands through a skill | `setup --agent claude` |
| **Codex** | MCP tools from `clojure-lite-lsp mcp` | `setup --agent codex` |
| **Any agent** | `clojure-lite-lsp query …` in a shell | nothing: mention it in the agent's instructions |

## Set up a project

In the project's directory:

```sh
clojure-lite-lsp setup --agent claude        # or codex, or claude,codex
```

It writes what the agent needs into the project, tells the agent's own CLI
about the server where that's how the agent works, and indexes the project, so
the first question is answered at once. It's safe to run again: it replaces its
own parts and keeps everything else. `--no-index` skips the indexing; a
directory argument sets up another project.

- [Claude Code](claude-code.md): what it sets up and how Claude uses it.
- [Codex](codex.md): the same for Codex.
- [Query commands](../guide/query.md): what agents (and you) can ask.
