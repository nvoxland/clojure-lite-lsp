(ns clojure-lite-lsp.home
  "Where clojure-lite-lsp keeps things: in each OS's own places, caches (the
  index, extracted sources, config copies, logs: all rebuilt when missing)
  apart from data (the Claude Code plugin marketplace, which Claude
  records the path of). $CLOJURE_LITE_LSP_HOME puts all of it in one dir."
  (:require
   [clojure-lite-lsp.schema :as schema]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private app "clojure-lite-lsp")

(defn- system []
  {:os (System/getProperty "os.name")
   :user-home (System/getProperty "user.home")
   :env (System/getenv)})

(defn- os [{:keys [os]}]
  (let [os (str/lower-case (str os))]
    (cond (str/starts-with? os "mac") :macos
          (str/starts-with? os "windows") :windows
          :else :unix)))

(defn- xdg
  "XDG base directory `var`, else `default` under the user's home. The spec
  says to ignore a relative path."
  [{:keys [env user-home]} var & default]
  (let [v (get env var)]
    (if (and (not (str/blank? v)) (.isAbsolute (io/file v)))
      (str (io/file v app))
      (str (apply io/file user-home (concat default [app]))))))

(defn- windows [{:keys [env user-home]} kind]
  (str (io/file (or (get env "LOCALAPPDATA") (str (io/file user-home "AppData" "Local"))) app kind)))

(defn dir
  "The home dir, for caches: $CLOJURE_LITE_LSP_HOME, else the OS's cache
  dir (~/Library/Caches on macOS, $XDG_CACHE_HOME or ~/.cache elsewhere,
  %LOCALAPPDATA% on Windows)."
  ([] (dir (system)))
  ([sys]
   (or (get (:env sys) "CLOJURE_LITE_LSP_HOME")
       (case (os sys)
         :macos (str (io/file (:user-home sys) "Library" "Caches" app))
         :windows (windows sys "Cache")
         (xdg sys "XDG_CACHE_HOME" ".cache")))))

(defn data-dir
  "The dir for what isn't a cache: $CLOJURE_LITE_LSP_HOME, else the OS's
  data dir (~/Library/Application Support on macOS, $XDG_DATA_HOME or
  ~/.local/share elsewhere, %LOCALAPPDATA% on Windows)."
  ([] (data-dir (system)))
  ([sys]
   (or (get (:env sys) "CLOJURE_LITE_LSP_HOME")
       (case (os sys)
         :macos (str (io/file (:user-home sys) "Library" "Application Support" app))
         :windows (windows sys "Data")
         (xdg sys "XDG_DATA_HOME" ".local" "share")))))

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
