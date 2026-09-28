(ns clojure-lite-lsp.index-fixture
  "Indexing for tests: temp indexers and writers, analyzing code, syncing
  projects, and what the index then holds."
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.kinds :as kinds]
   [clojure-lite-lsp.normalize :as normalize]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]))

;;;; analysis without an index

(defn kondo
  "Run clj-kondo on `code` as file `filename` with the options
  clojure-lite-lsp uses."
  [code filename & {:keys [external?]}]
  (with-in-str code
    (kondo/run! {:lint ["-"]
                 :filename filename
                 :lang (keyword (re-find #"[^.]+$" filename))
                 :cache false
                 :skip-lint external?
                 :config {:output {:canonical-paths true}
                          :linters normalize/kept-linters
                          :analysis (if external?
                                      normalize/dependency-analysis-options
                                      normalize/project-analysis-options)}})))

(defn analyzed
  "The normalized elements of `code` analyzed as `filename`."
  [code filename & {:keys [external?] :as opts}]
  (-> (kondo code filename opts)
      (normalize/normalize {:external? external?})
      (get filename)))

(defn unit-key
  "A unit key for `content`, with `more` keys."
  [content & {:as more}]
  (merge {:content-hash (.getBytes (str content)) :lang-key "clj" :kondo-version "test"
          :config-hash (byte-array 1) :options-hash (byte-array 1) :external? false}
         more))

;;;; writing directly

(defmacro with-writer
  "Run body with `w` a writer on a fresh index, and `c` its connection."
  [[w c] & body]
  `(with-open [~c (db/open-writer (tu/temp-db-path))]
     (let [~w (writer/writer ~c)]
       ~@body)))

(defn file!
  "Write `code` as file `path` of project `p`: its unit id."
  [w p path code]
  (let [[u] (writer/write-units! w [[(unit-key code) (analyzed code "x.clj")]])]
    (snapshot/set-file-unit! w p path u {:ord 0})
    u))

(defn count-of
  "count(*) of `from`: a table, possibly with a WHERE clause and its
  `params`."
  [c from & params]
  (apply db/query-value c (str "SELECT count(*) FROM " from) params))

;;;; indexing

(defn temp-indexer
  "An indexer on a fresh index, with `opts` (clojure-lite-lsp.indexer/indexer's)."
  ^java.io.Closeable [& {:as opts}]
  (indexer/indexer (merge {:db-path (tu/temp-db-path) :cache-dir (tu/temp-dir)} opts)))

(defn sync-project!
  "Index the project at `root` until the queue is empty: its id."
  [ix root]
  (let [c (:c ix)
        p (snapshot/ensure-project! c root)]
    (queue/enqueue! c p :sync "" 1)
    (indexer/run-until-idle! ix)
    p))

(defn visible-defs
  "The var definitions project `p` can see, as #{\"ns/name\"}."
  [c p]
  (set (map first (db/query c "SELECT n.text || '/' || m.text FROM definition d
                               JOIN project_unit pu ON pu.unit_id = d.unit_id AND pu.project_id = ?
                               JOIN sym n ON n.id = d.ns JOIN sym m ON m.id = d.name
                               WHERE d.kind = ?"
                            p (kinds/code :var-def)))))

(defn analyzed-files
  "Re-sync the project at `root` after `change`: the names of the files
  analyzed."
  [ix root change]
  (change)
  (let [seen (atom #{})
        analyze-files analyze/analyze-files]
    (with-redefs [analyze/analyze-files (fn [paths opts]
                                          (swap! seen into (map #(.getName (io/file ^String %)) paths))
                                          (analyze-files paths opts))]
      (sync-project! ix root))
    @seen))

(defn project-using
  "A project whose deps.edn uses the given jars, with one source file. A
  jar is a path, or [lib path] for jars with Maven metadata: tools.deps
  reads the poms inside local jars, so a jar depending on acme/y resolves
  only when acme/y is itself declared under that name."
  [jars]
  (tu/project! {"deps.edn" (pr-str {:paths ["src"]
                                    :deps (into {}
                                                (map-indexed (fn [i j]
                                                               (if (vector? j)
                                                                 [(first j) {:local/root (second j)}]
                                                                 [(symbol "dep" (str "d" i)) {:local/root j}])))
                                                jars)})
                "src/app/core.clj" (str "(ns app.core) (defn own-" (hash jars) " [] 1)")}))
