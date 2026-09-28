(ns clojure-lite-lsp.daemon-fixture
  "Daemons for tests, running in this process."
  (:require
   [clojure-lite-lsp.client :as client]
   [clojure-lite-lsp.daemon :as daemon]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.version :as version]
   [clojure.java.io :as io])
  (:import
   [clojure.lang IBlockingDeref IDeref IPending]
   [java.io Closeable]
   [java.sql SQLException]))

(defn home
  "A fresh home dir."
  []
  (str (tu/temp-dir)))

(def fast
  "Daemon options for tests: quick to notice work, slow to go idle."
  {:poll-ms 20 :idle-exit-ms 60000 :gc-after-idle-ms 60000 :version "test"})

(defn client-db
  "A client connection to the index of home `h`."
  [h]
  (db/open-client (:db (home/paths h))))

(defn- registered?
  "Has a daemon registered in the index of home `h`?"
  [h]
  (with-open [c (client-db h)]
    (try (some? (db/query-value c "SELECT version FROM daemon WHERE id = 1"))
         (catch SQLException _ false))))

(defn stop!
  "Ask home `h`'s daemon to stop, and wait for the daemons `ds` (derefable):
  whether they all ended."
  [h ds]
  (with-open [c (client-db h)] (client/request-stop! c))
  (every? #(not= ::timeout (deref % 30000 ::timeout)) ds))

(defn start!
  "Run a daemon for home `h` in this process, returning once it is up (as
  clients do through ensure-daemon!: the daemon creates the schema). The
  handle derefs to how it ended, and stops it when closed (`with-open`)."
  [h opts]
  (.mkdirs (io/file (:dir (home/paths h))))
  (let [d (future (daemon/serve! (merge fast {:home h} opts)))]
    (when-not (tu/eventually #(or (realized? d) (registered? h)))
      (throw (ex-info "The daemon didn't start" {:home h})))
    (reify
      IDeref
      (deref [_] @d)
      IBlockingDeref
      (deref [_ ms timeout-value] (deref d ms timeout-value))
      IPending
      (isRealized [_] (realized? d))
      Closeable
      (close [_]
        (when-not (stop! h [d])
          (throw (ex-info "The daemon didn't end" {:home h})))))))

(defn in-process-opts
  "Client options (clojure-lite-lsp.client/ensure-daemon!'s) for home `h`
  whose daemons run in this process, each added to atom `daemons`."
  [h daemons]
  {:home h
   :spawn! #(swap! daemons conj (future (daemon/serve! (merge fast {:home h :version version/version}))))})

(defmacro with-in-process-daemons
  "Run body with `opts` client options for a fresh home `h` whose daemons
  run in this process (`in-process-opts`), collected in atom `daemons`;
  stop them after."
  [[opts h daemons] & body]
  `(let [h# (home)
         daemons# (atom [])
         ~opts (in-process-opts h# daemons#)
         ~h h#
         ~daemons daemons#]
     (try
       ~@body
       (finally
         (when (seq @daemons#) (stop! h# @daemons#))))))
