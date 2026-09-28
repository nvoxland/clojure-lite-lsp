(ns clojure-lite-lsp.log
  "Messages for the log: stderr, which is daemon.log for the indexer and
  the editor's server log for clojure-lite-lsp lsp (whose stdout is the
  LSP connection).")

(defn warn
  "Print `xs` to stderr, as one clojure-lite-lsp line."
  [& xs]
  (binding [*out* *err*]
    (apply println "clojure-lite-lsp:" xs)))
