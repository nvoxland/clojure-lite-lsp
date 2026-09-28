(ns clojure-lite-lsp.home
  "Where things live under a clojure-lite-lsp home dir (default
  ~/.cache/clojure-lite-lsp)."
  (:require
   [clojure-lite-lsp.schema :as schema]
   [clojure.java.io :as io]))

(defn paths
  "The files under home dir `home`. The index, its daemon's locks and log
  are per schema version (v<n>/): clojure-lite-lsp versions with different
  schemas run side by side instead of rebuilding each other's index.
  Clojure configs and extracted sources are content-addressed, so shared."
  ([home] (paths home schema/version))
  ([home schema-version]
   (let [dir (io/file home (str "v" schema-version))]
     {:home (str home)
      :dir (str dir)
      :db (str (io/file dir "index.db"))
      :daemon-lock (str (io/file dir "daemon.lock"))
      :spawn-lock (str (io/file dir "spawn.lock"))
      ;; every clojure-lite-lsp lsp holds a shared lock on it while it runs
      :clients-lock (str (io/file dir "clients.lock"))
      :log (str (io/file dir "daemon.log"))})))
