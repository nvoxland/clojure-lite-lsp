# Install

clojure-lite-lsp is a single native binary, `clojure-lite-lsp`. Download a
release, or build it from source.

Either way, projects need their build tool: the
[Clojure CLI](https://clojure.org/guides/install_clojure) for `deps.edn`,
`lein` for `project.clj`, `bb` for `bb.edn`.

## Download a release

[Releases](https://github.com/nvoxland/clojure-lite-lsp/releases) have
binaries for macOS (Apple silicon) and Linux (x86-64 and ARM64). Pick yours
and put it on your `PATH`:

```sh
version=0.1.0
target=macos-aarch64   # or linux-x86_64, linux-aarch64
curl -fsSLO "https://github.com/nvoxland/clojure-lite-lsp/releases/download/v$version/clojure-lite-lsp-$version-$target.tar.gz"
tar -xzf "clojure-lite-lsp-$version-$target.tar.gz"
mkdir -p ~/.local/bin
mv "clojure-lite-lsp-$version-$target/clojure-lite-lsp" ~/.local/bin/
```

Downloaded with a browser instead, macOS quarantines the binary, which
isn't signed: `xattr -d com.apple.quarantine ~/.local/bin/clojure-lite-lsp`
lets it run.

## Build from source

This needs [GraalVM](https://www.graalvm.org/) 25 or later (`native-image`),
as `JAVA_HOME` or its `native-image` on `PATH`, and the Clojure CLI.

```sh
git clone https://github.com/nvoxland/clojure-lite-lsp
cd clojure-lite-lsp
JAVA_HOME=/path/to/graalvm bin/install-server
```

`bin/install-server` builds `target/clojure-lite-lsp` and links it as
`~/.local/bin/clojure-lite-lsp`. After pulling changes, rebuild with
`bin/install-server --rebuild`.

## Check it

Make sure `~/.local/bin` is on your `PATH`, then:

```sh
clojure-lite-lsp version
```

## Next

- [First steps](first-steps.md): index a project and ask it something.
- Set up your editor: [Zed](../editors/zed.md), [VS Code](../editors/vscode.md)
  or [another editor](../editors/other.md).
- Set up an agent: [Claude Code or Codex](../agents/index.md).
