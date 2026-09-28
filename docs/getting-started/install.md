# Install

clojure-lite-lsp is a single native binary, `clojure-lite-lsp`.

## Get the binary

Download the archive for your platform from the
[latest release](https://github.com/nvoxland/clojure-lite-lsp/releases/latest):

| Platform | Archive |
|---|---|
| macOS (Apple silicon) | `clojure-lite-lsp-<version>-macos-aarch64.tar.gz` |
| Linux (x86-64) | `clojure-lite-lsp-<version>-linux-x86_64.tar.gz` |
| Linux (ARM64) | `clojure-lite-lsp-<version>-linux-aarch64.tar.gz` |

Each archive has a `.sha256` checksum beside it. Unpack it:

```sh
tar -xzf clojure-lite-lsp-<version>-<platform>.tar.gz
```

To build it from source instead, see [DEV.md](https://github.com/nvoxland/clojure-lite-lsp/blob/main/DEV.md).

## Put it on your PATH

Move or link `clojure-lite-lsp` into a directory on your `PATH`. Agents and
editors find it there. An editor started from a desktop launcher may not see
the same `PATH` as your shell; if it can't find the binary, its settings take
the full path instead ([VS Code](editors/vscode.md#settings),
[Zed](editors/zed.md#settings)).

The binary isn't signed. On macOS, a copy downloaded with a browser is
quarantined; `xattr -d com.apple.quarantine <path>/clojure-lite-lsp` lets it
run.

## Check it

```sh
clojure-lite-lsp version
```

## Next

- Try it: `clojure-lite-lsp query` in a project lists what it can
  [look up](../guide/query.md).
- Set up an agent: [Claude Code or Codex](agents/index.md).
- Set up your editor: [VS Code](editors/vscode.md), [Zed](editors/zed.md)
  or [another editor](editors/other.md).
