# Other editors

Any editor with an LSP client can use clojure-lite-lsp: run `clojure-lite-lsp lsp`
over stdio for Clojure files. A project is found from the workspace folders
(or root) the editor sends.

!!! note
    These configurations follow each editor's standard LSP setup, but haven't
    been tested by the project yet. Corrections are welcome.

## Neovim

Neovim 0.11 and later have an LSP client built in:

```lua
vim.lsp.config('clojure_lite_lsp', {
  cmd = { 'clojure-lite-lsp', 'lsp' },
  filetypes = { 'clojure', 'edn' },
  root_markers = { 'deps.edn', 'project.clj', 'bb.edn', '.git' },
})
vim.lsp.enable('clojure_lite_lsp')
```

## Emacs

With Eglot (built in since Emacs 29):

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurescript-mode clojurec-mode clojure-ts-mode)
                 . ("clojure-lite-lsp" "lsp"))))
```

Then `M-x eglot` in a Clojure buffer, or add `eglot-ensure` to
`clojure-mode-hook`.

## Helix

In `languages.toml`:

```toml
[language-server.clojure-lite-lsp]
command = "clojure-lite-lsp"
args = ["lsp"]

[[language]]
name = "clojure"
language-servers = ["clojure-lite-lsp"]
```

## Sublime Text

With the [LSP](https://packagecontrol.io/packages/LSP) package, in its settings:

```json
{
  "clients": {
    "clojure-lite-lsp": {
      "enabled": true,
      "command": ["clojure-lite-lsp", "lsp"],
      "selector": "source.clojure"
    }
  }
}
```

## Initialization options

| Option | Default | |
|---|---|---|
| `dependency-scheme` | `"file"` | How library sources are returned: `"file"` (extracted, read-only files), or `"jar"` / `"zipfile"` URIs for editors that open those. |
| `waitForIndex` | `false` | Hold requests until the project's index is up to date, instead of answering from what's indexed so far. For agents. |
