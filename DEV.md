# Developing clojure-lite-lsp

How to build clojure-lite-lsp from source and work on it. For what it does and
how to use it, see the [documentation](https://nvoxland.github.io/clojure-lite-lsp/);
for how it's put together, [Architecture](https://nvoxland.github.io/clojure-lite-lsp/architecture/).

## Requirements

- [GraalVM](https://www.graalvm.org/) 25 or later, for `native-image`: as
  `JAVA_HOME`, or its `native-image` on `PATH`. Running from source and the
  tests only need a JDK (CI uses 25).
- The [Clojure CLI](https://clojure.org/guides/install_clojure).
- For the editor extensions: Node.js (VS Code) and Rust through rustup (Zed).
- For the docs site: Python 3.11+ and [Poetry](https://python-poetry.org/).

## Layout

| | |
|---|---|
| `src/`, `test/` | The server, indexer and command line, and their tests |
| `bin/` | Build, install and smoke-test scripts |
| `vscode/` | The VS Code extension |
| `zed/` | The Zed extension |
| `docs/`, `overrides/`, `mkdocs.yml` | The documentation site |
| `.github/workflows/` | CI, releases and the docs site |

## Build

```sh
bin/build-native              # builds target/clojure-lite-lsp
bin/install-server            # builds it if missing, and links ~/.local/bin/clojure-lite-lsp
bin/install-server --rebuild  # always builds first
```

The build takes a minute or two.

- `CLOJURE_LITE_LSP_MARCH` sets the CPU target (default `native`, the build
  machine's). Portable builds use e.g. `x86-64-v2` or `armv8.1-a`, at the cost
  of slower content hashing on ARM.
- `CLOJURE_LITE_LSP_BIN_DIR` sets where `bin/install-server` links the binary
  (default `~/.local/bin`).

`bin/smoke-test [binary] [expected-version]` checks a built binary: it runs,
indexes this repository and answers a query, and its language server answers
`initialize`. It uses a throwaway index.

## Run from source

Without a native build, the JVM runs the same commands:

```sh
clojure -M -m clojure-lite-lsp.main query definition clojure-lite-lsp.query/definition
```

Started this way, it starts its indexer on the JVM too.

To try a build without touching your own index, point
`CLOJURE_LITE_LSP_HOME` somewhere else:

```sh
CLOJURE_LITE_LSP_HOME=/tmp/clojure-lite-lsp-try target/clojure-lite-lsp status
```

## Tests

```sh
clojure -M:test
```

Or from a REPL, which is faster when iterating: `clojure -M:nrepl` starts an
nREPL server with `test/` on the classpath. The suite takes a few minutes: many
tests index small projects for real, and some start indexer processes.

Test helpers live in namespaces without tests: `test-util` (temporary
directories, projects and jars), `index-fixture`, `daemon-fixture` and
`query-fixture`.

## Lint and format

CI fails on either:

```sh
clj-kondo --lint src test --fail-level warning
clojure -Sdeps '{:deps {dev.weavejester/cljfmt {:mvn/version "0.13.1"}}}' -M -m cljfmt.main check src test
```

(`fix` instead of `check` reformats.)

Every namespace sets `*warn-on-reflection*`, and a reflection warning fails
the native build in CI: a reflective call compiles fine, then fails at runtime
in the native binary.

## Index format versions

The index is a cache, rebuilt rather than migrated:

- Changing the database schema (`clojure-lite-lsp.schema`) means bumping
  `schema/version`. Each version gets its own index directory, so versions
  side by side don't rebuild each other's.
- Changing what analysis produces (`clojure-lite-lsp.normalize`) means
  bumping `normalize/version`: it's part of every analysis key, so everything
  is analyzed again.

## Editor extensions

VS Code, in `vscode/`:

```sh
npm ci && npm test   # unit tests
npm run test:e2e     # end to end, in a real VS Code (needs the binary on PATH)
./install            # build, package and install the extension
```

Zed, in `zed/`:

```sh
cargo test
```

Install it with **zed: install dev extension**, selecting `zed/`. After
changing and reinstalling it, run **editor: restart language server**: Zed
stops the server when it reloads the extension, and doesn't start it again.

## Docs

```sh
poetry install --with docs
poetry run mkdocs serve       # http://127.0.0.1:8000/clojure-lite-lsp/, reloads on changes
```

Pushing changes to the docs to `main` publishes the site.

## CI and releases

CI (`.github/workflows/ci.yml`) runs on every push and pull request: lint and
format, the tests on Linux and macOS, the native build and smoke test on
both, and the extensions' tests.

To release:

1. Set the version in `src/clojure_lite_lsp/version.clj` (no `-SNAPSHOT`) and
   commit.
2. `git tag vX.Y.Z && git push origin main vX.Y.Z`. The release workflow checks
   the tag against `version.clj`, runs the tests, builds and smoke-tests
   binaries for Linux (x86-64, ARM64) and macOS (Apple silicon), and publishes
   them as a GitHub Release.
3. Set `version.clj` to the next `X.Y.Z-SNAPSHOT` and commit. A newer daemon
   replaces an older one, so builds from source mustn't look older than the
   release.

Running the release workflow by hand from the Actions tab is a dry run: it
builds and tests everything and publishes nothing.
