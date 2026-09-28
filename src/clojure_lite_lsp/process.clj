(ns clojure-lite-lsp.process
  "Running other programs: build tools for classpaths, agents' CLIs,
  macOS's java_home."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io IOException]
   [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

(defn run
  "Run `cmd` (program and arguments) and wait for it: {:exit :out :err}.
  Exit 127 when it can't be started, 124 when it didn't end within
  `timeout-ms` (it's killed). Its stdin is closed: nothing waits on input
  that can't come (and clojure-lite-lsp lsp's own stdin is the LSP
  connection). `merge-err?` puts its stderr in :out."
  [cmd {:keys [dir timeout-ms merge-err?] :or {timeout-ms 120000}}]
  (try
    (let [p (.start (cond-> (ProcessBuilder. ^java.util.List (vec cmd))
                      dir (.directory (io/file dir))
                      merge-err? (.redirectErrorStream true)))
          _ (.close (.getOutputStream p))
          out (future (slurp (.getInputStream p)))
          err (future (slurp (.getErrorStream p)))]
      (if (.waitFor p (long timeout-ms) TimeUnit/MILLISECONDS)
        {:exit (.exitValue p) :out @out :err @err}
        (do (.destroyForcibly p)
            {:exit 124 :out "" :err (str (str/join " " cmd) " didn't finish in " (quot timeout-ms 1000) " s")})))
    (catch IOException e
      {:exit 127 :out "" :err (ex-message e)})))
