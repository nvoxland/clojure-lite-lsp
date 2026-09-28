# VS Code extension: clojure-lite-lsp

Runs `clojure-lite-lsp lsp` as the language server for Clojure files. VS Code's
built-in Clojure support provides the language (syntax highlighting); this
adds navigation, references, hover, symbols and call hierarchy.

## Install

1. [Install the binary](https://nvoxland.github.io/clojure-lite-lsp/getting-started/install/),
   so `clojure-lite-lsp` is on your PATH (or [build it](../DEV.md)).

2. Build and install the extension (needs Node.js):

   ```sh
   vscode/install
   ```

   It packages `vscode/clojure-lite-lsp-<version>.vsix` and installs it with
   VS Code's `code` command. Without that command, install the `.vsix` with
   **Extensions: Install from VSIX…**.

3. Reload the window and open a `.clj` file. Indexing progress shows in the
   status bar; the first index of a big project takes a while (about 35 s for
   5,000 source files), another worktree of it a few seconds.

### With Calva

Calva starts its own language server, clojure-lsp, and every result would
show twice. Keep Calva for the REPL and turn its clojure-lsp off:

```json
"calva.enableClojureLspOnStart": "never"
```

## Settings

- `clojure-lite-lsp.path`: the executable. Empty (the default): found on PATH.
- `clojure-lite-lsp.args`: default `["lsp"]`.
- `clojure-lite-lsp.env`: added to the environment, e.g. a separate index
  while trying it out:

  ```json
  "clojure-lite-lsp.env": { "CLOJURE_LITE_LSP_HOME": "/tmp/clojure-lite-lsp-try" }
  ```

Changing a setting restarts the server; so does **clojure-lite-lsp: Restart
the language server**.

## Using it

- Go to definition into a library opens its source as a read-only file under
  `sources/` in clojure-lite-lsp's cache directory, fully navigable once open.
- `clojure-lite-lsp status` in a terminal shows the indexer, the index, and
  each project's files and jars.
- The server's own log is in **Output → clojure-lite-lsp**; the indexer logs
  to `v<n>/daemon.log` in its cache directory
  ([where that is](https://nvoxland.github.io/clojure-lite-lsp/guide/configuration/#where-things-live)).
- A project's classpath comes from `deps.edn` (aliases `:dev` and `:test` by
  default), `project.clj` or `bb.edn`. A `.clojure-lite-lsp.edn` at the
  project root can change that; see
  [Configuration](https://nvoxland.github.io/clojure-lite-lsp/guide/configuration/).

## Development

- `npm test`: unit tests of how the server is found and launched.
- `npm run test:e2e`: end to end, in a real VS Code (downloaded into
  `.vscode-test/`, with its own profile and a throwaway index): opens a small
  project and checks definition, hover, highlights and signature help. Needs `clojure-lite-lsp` and the Clojure CLI on PATH.
