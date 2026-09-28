(ns clojure-lite-lsp.query-fixture
  "Index small projects for query tests. Every fixture in a run shares one
  index, so jars (Clojure itself) are analyzed once and then only linked,
  thanks to content addressing."
  (:require
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.indexer :as indexer]
   [clojure-lite-lsp.query :as q]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.test-util :as tu :refer [project!]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defonce ^:private shared
  (delay (let [path (tu/temp-db-path)]
           {:db-path path
            :indexer (indexer/indexer {:db-path path :cache-dir (tu/temp-dir)})})))

(defn index!
  "Index a project made of `files` ({relative-path code}, deps.edn added
  unless given). Returns {:root :p :c}, :c a read-only connection."
  [files]
  (let [{:keys [db-path indexer]} @shared
        root (project! (merge tu/src-deps files))
        p (snapshot/ensure-project! (:c indexer) root)]
    (queue/enqueue! (:c indexer) p :sync "" 1)
    (indexer/run-until-idle! indexer)
    {:root root :p p :c (db/open-reader db-path)}))

(defmacro with-project
  "Bind `sym` to an indexed project of `files` for the body."
  [[sym files] & body]
  `(let [~sym (index! ~files)]
     (try ~@body (finally (.close ^java.sql.Connection (:c ~sym))))))

(defn path [{:keys [root]} rel] (str root "/" rel))

(defn at
  "The [row col] (1-based) of the first character of the `n`th (default
  0th) occurrence of `needle` in file `rel`."
  ([proj rel needle] (at proj rel needle 0))
  ([proj rel needle n]
   (let [text (slurp (io/file (path proj rel)))
         idx (loop [from 0 k n]
               (let [i (str/index-of text needle from)]
                 (when-not i (throw (ex-info (str "Not found: " needle) {:rel rel :n n})))
                 (if (zero? k) i (recur (inc i) (dec k)))))
         before (subs text 0 idx)]
     [(inc (count (re-seq #"\n" before)))
      (inc (- idx (inc (or (str/last-index-of before "\n") -1))))])))

(defn rel
  "A location, made readable: its path relative to the project root (or
  jar entry), and its start."
  [{:keys [root]} {:keys [path entry pos]}]
  (when path
    [(if entry entry (subs path (inc (count root)))) (vec (take 2 pos))]))

(defn locs
  "Where the `n`th (default 0th) occurrence of each `needle` starts, from
  [file needle n] specs: a set of [file [row col]], as `ask` answers."
  [proj & specs]
  (set (for [[file needle n] specs] [file (at proj file needle (or n 0))])))

(defn ask
  "Run (query c p path row col & args) at the `n`th (default 0th)
  occurrence of `needle` in `file`, `offset` columns into it: the
  locations it answers, as a set of [file [row col]]."
  [proj query file needle & {:keys [n offset args] :or {n 0 offset 0}}]
  (let [[row col] (at proj file needle n)]
    (set (map #(rel proj %) (apply query (:c proj) (:p proj) (path proj file) row (+ col offset) args)))))

(defn definition [proj file needle & {:as opts}] (ask proj q/definition file needle opts))
(defn declaration [proj file needle & {:as opts}] (ask proj q/declaration file needle opts))
(defn implementations [proj file needle & {:as opts}] (ask proj q/implementations file needle opts))

(defn references
  "`ask` for references; `include-declaration?` as LSP's."
  [proj file needle & {:keys [include-declaration?] :as opts}]
  (ask proj q/references file needle (assoc opts :args [{:include-declaration? include-declaration?}])))
