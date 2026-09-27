# Install

clojure-lite-lsp is a single native binary, `clojure-lite-lsp`. For now it's
built from source.

## Requirements

- [GraalVM](https://www.graalvm.org/) 25 or later (`native-image`), to build.
- The [Clojure CLI](https://clojure.org/guides/install_clojure), for
  `deps.edn` projects. Leiningen and Babashka projects need `lein` or `bb`.

## Build and install

```sh
git clone https://github.com/nvoxland/clojure-lite-lsp
cd clojure-lite-lsp
JAVA_HOME=/path/to/graalvm bin/install-server
```

`bin/install-server` builds `target/clojure-lite-lsp` and links it as
`~/.local/bin/clojure-lite-lsp`. Make sure `~/.local/bin` is on your `PATH`.
After pulling changes, rebuild with `bin/install-server --rebuild`.

Check it:

```sh
clojure-lite-lsp version
```

## Next

- [First steps](first-steps.md): index a project and ask it something.
- Set up your editor: [Zed](../editors/zed.md), [VS Code](../editors/vscode.md)
  or [another editor](../editors/other.md).
- Set up an agent: [Claude Code or Codex](../agents/index.md).
