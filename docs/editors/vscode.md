# VS Code

The VS Code extension in [`vscode/`](https://github.com/nvoxland/clojure-lite-lsp/tree/main/vscode)
runs clojure-lite-lsp as the language server for VS Code's built-in Clojure
language.

## Install

1. [Install the binary](../getting-started/install.md), so `clojure-lite-lsp` is on your `PATH`.
2. Build and install the extension (needs Node.js):

    ```sh
    vscode/install
    ```

    It packages a `.vsix` and installs it with VS Code's `code` command. Without
    that command, install the `.vsix` with **Extensions: Install from VSIX…**.

3. Reload the window and open a `.clj` file.

!!! tip "Using Calva?"
    Calva starts clojure-lsp on its own, and every result would show twice.
    Keep Calva for the REPL and turn its clojure-lsp off:

    ```json
    "calva.enableClojureLspOnStart": "never"
    ```

## Settings

| Setting | Default | |
|---|---|---|
| `clojure-lite-lsp.path` | (on `PATH`) | The executable. |
| `clojure-lite-lsp.args` | `["lsp"]` | Arguments that start the server. |
| `clojure-lite-lsp.env` | `{}` | Added to the environment, e.g. `CLOJURE_LITE_LSP_HOME`. |

Changing a setting restarts the server, as does **clojure-lite-lsp: Restart the
language server**. The server's log is in **Output → clojure-lite-lsp**.
