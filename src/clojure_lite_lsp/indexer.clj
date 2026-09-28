(ns clojure-lite-lsp.indexer
  "The daemon's work: drain the queue, keeping each project's snapshot and
  the analysis it needs up to date.

  - :sync   recompute the project's classpath, configs and file list; link
            everything whose unit or jar already exists (content addressing
            makes an unchanged file or a jar another project indexed cost
            nothing) and enqueue the rest
  - :file   analyze project or external-dir files (skipping unchanged ones)
  - :delete forget files
  - :jar    analyze jars
  - :dep-file fully analyze library files someone opened (extracted by
            clojure-lite-lsp.sources)

  Analysis is sharded across concurrent clj-kondo runs (clojure-lite-lsp.analyze), and
  pipelined: while one :file or :jar batch is written, the next one is
  already being analyzed. Analyses themselves never overlap: hooks share
  clj-kondo's process-wide state, and batches can have different configs.
  Every database access (preparing a batch, writing one) still happens on
  the loop thread, one after the other: only analysis, which touches no
  database, runs alongside."
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.fingerprint :as fingerprint]
   [clojure-lite-lsp.home :as home]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.kondo-hooks :as kondo-hooks]
   [clojure-lite-lsp.log :as log]
   [clojure-lite-lsp.ns-analysis :as nsa]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.reuse :as reuse]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.sources :as sources]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io Closeable File]
   [java.sql Connection]
   [java.util.concurrent ExecutionException]
   [org.sqlite SQLiteErrorCode SQLiteException]))

(set! *warn-on-reflection* true)

(defrecord Indexer [c w reader home shards contexts batch-sizes in-flight reuser requeued]
  Closeable
  (close [_]
    (when (realized? reader) (.close ^Connection @reader))
    (.close ^Connection c)))

(defn indexer
  "An indexer writing to the index at `db-path`, keeping clj-kondo configs
  (and extracted sources) under home dir `home`."
  [{:keys [db-path home shards batch-sizes] :or {shards analyze/default-shards}}]
  (let [c (db/open-writer db-path)
        home (or home (home/dir))]
    (map->Indexer {:c c :w (writer/writer c)
                   ;; for analysis, which runs beside the loop thread's writes
                   :reader (delay (db/open-reader db-path))
                   :home home
                   :shards shards
                   :batch-sizes batch-sizes
                   :contexts (atom {})
                   ;; the batch being analyzed: {:batch :job}
                   :in-flight (atom nil)
                   :reuser (reuse/reuser home)
                   ;; {[project path] [unit answers]}: see recheck-dependents!
                   :requeued (atom {})})))

;;;; project context: what analysis of a project's files and jars needs

(def ^:private source-extensions #"\.(clj|cljs|cljc|edn|bb)$")

(defn- source-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile ^File %))
       (filter #(re-find source-extensions (.getName ^File %)))
       (mapv #(.getCanonicalPath ^File %))))

(defn- root-of [c p] (db/query-value c "SELECT root FROM project WHERE id = ?" p))

(defn- compute-context [{:keys [c home]} p]
  (let [root (root-of c p)
        entries (classpath/memoized! c p root)
        by-kind (group-by :kind entries)
        jars (mapv :path (:jar by-kind))]
    {:root root
     :source-dirs (mapv :path (:source-dir by-kind))
     :external-dirs (mapv (fn [{:keys [path ord]}] {:path path :ord ord :config (kc/dir-config! home path)})
                          (:external-dir by-kind))
     :jars (mapv (juxt :ord :path) (:jar by-kind))
     :project-config (kc/project-config! home root entries)
     :jar-context (kc/jar-context jars)}))

(defn- context
  "Project `p`'s context, computed once per sync (or after a restart)."
  [{:keys [contexts] :as ix} p]
  (or (@contexts p)
      (let [ctx (compute-context ix p)]
        (swap! contexts assoc p ctx)
        ctx)))

(defn- under? [dir path] (str/starts-with? path (str dir File/separator)))

(defn- file-placement
  "Where `path` belongs in project `p`: {:ord :external? :mode :config},
  or nil when it's in none of the project's source or external dirs."
  [{:keys [root source-dirs external-dirs project-config]} path]
  (let [own {:ord 0 :external? false :mode :project :config project-config}]
    (or (when (some #(under? % path) source-dirs) own)
        (some (fn [{dir :path :keys [ord config]}]
                (when (under? dir path) {:ord ord :external? true :mode :dependency :config config}))
              external-dirs)
        ;; anywhere else under the root (build.clj, scripts/): when asked
        ;; for (an editor opened it), as the project's own
        (when (under? root path) own))))

(defn- file-unit-key! [{:keys [c]} {:keys [mode config]} path]
  (when-let [h (fingerprint/content-hash! c path)]
    (analyze/unit-key mode config h path)))

(defn- serve-fn
  "Answers for hooks asking about a namespace (clojure-lite-lsp.ns-analysis), as project
  `p` sees it: for analysis, off the loop thread."
  [{:keys [reader]} p]
  (fn [lang ns-sym]
    (let [r @reader]
      ;; one reader connection, shared by the concurrent clj-kondo runs
      (locking r (nsa/index-answer r p lang ns-sym)))))

(defn- digest-fn
  "Digests of today's answers in project `p`, on the loop thread."
  [{:keys [c]} p]
  (memoize (fn [lang ns-sym]
             (nsa/digest (kondo-hooks/answer (partial nsa/index-answer c p) lang ns-sym)))))

(defn- existing-unit!
  "The unit for a file's key: analyzed under this config, or under another
  whose differences don't touch what the file references (clojure-lite-lsp.reuse), and
  whose hooks' answers `digest-of` finds still hold."
  [{:keys [w reuser]} unit-key digest-of]
  (when unit-key (reuse/unit-for! reuser w unit-key digest-of)))

(def ^:private requeues-remembered
  "How many (unit, answers) pairs of a file the requeue guard remembers."
  8)

(defn first-requeue!
  "Record that a file (`k`) whose analysis doesn't match its hooks'
  answers is queued again for `entry` [unit answers]: true the first time
  for that entry. Had analyzing it given back one already seen, it would
  cycle forever. `seen` is an atom {k [entry ...]}."
  [seen k entry]
  (let [[before] (swap-vals! seen update k
                             (fn [entries]
                               (if (some #{entry} entries)
                                 entries
                                 (vec (take-last requeues-remembered (conj (vec entries) entry))))))]
    (not (some #{entry} (before k)))))

(defn- recheck-dependents!
  "Queue the files of project `p` whose hooks' answers no longer hold.
  `taken` is {path enqueued-at} of the file batch being written, whose
  rows are still queued: a file of it isn't waiting again unless queued
  since.

  A file is queued again for the same unit and the same answers only once:
  had analyzing it again given back that unit, nothing would change, and it
  would be queued forever."
  ([ix p] (recheck-dependents! ix p {}))
  ([{:keys [c requeued] :as ix} p taken]
   (when-let [marker (db/query-value c "SELECT id FROM sym WHERE text = ?" nsa/marker)]
     (let [digest-of (digest-fn ix p)
           waiting? (fn [path]
                      (when-let [at (db/query-value c "SELECT enqueued_at FROM pending
                                                       WHERE project_id = ? AND kind = 'file' AND path = ?"
                                                    p path)]
                        (not= at (taken path))))]
       (doseq [[u] (db/query c "SELECT r.unit_id FROM unit_ref r
                                JOIN project_unit pu ON pu.unit_id = r.unit_id AND pu.project_id = ?
                                WHERE r.ref = ?" p marker)
               :let [deps (reuse/ns-deps c u)
                     today (mapv (fn [[lang ns-sym]] (digest-of lang ns-sym)) deps)]
               :when (not= (map last deps) today)
               [path] (db/query c "SELECT path FROM project_file WHERE project_id = ? AND unit_id = ?" p u)]
         (cond
           (first-requeue! requeued [p path] [u today]) (queue/enqueue! c p :file path 1)
           ;; analyzed again and still not matching: that didn't help
           (not (waiting? path)) (log/warn "analysis of" path "doesn't match what its hooks are told; left as is")))))))

;;;; work

(def ^:private files-per-tx 500)

(defn- link-jars!
  "Link project `p`'s jars that are already indexed; queue the rest."
  [{:keys [c w home]} p {:keys [jars jar-context]}]
  (let [linked (vec (for [[ord path] jars
                          :let [config (kc/jar-config! home jar-context path)
                                h (fingerprint/content-hash! c path)]]
                      [ord path (snapshot/jar-id c (analyze/jar-key h config))]))]
    (snapshot/set-project-jars! w p linked)
    (doseq [[_ path jar-id] linked :when (nil? jar-id)]
      (queue/enqueue! c p :jar path 2))))

(defn- wanted-files
  "{path placement} of the files project `p` has: those in its dirs, and
  those outside, indexed because an editor opened them, while they exist."
  [{:keys [root source-dirs external-dirs] :as ctx} current]
  (let [listed (into {} (for [dir (concat source-dirs (map :path external-dirs))
                              path (source-files dir)]
                          [path (file-placement ctx path)]))
        kept? (fn [path] (and (not (listed path)) (under? root path) (.isFile (io/file path))
                              (re-find source-extensions path)))]
    (into listed (for [path (keys current) :when (kept? path)]
                   [path (file-placement ctx path)]))))

(defn- sync-project!
  [{:keys [c w contexts] :as ix} p]
  (swap! contexts dissoc p)
  (let [{:keys [project-config] :as ctx} (context ix p)
        current (snapshot/file-paths c p)
        wanted (wanted-files ctx current)
        unit-of (fn [digest-of path] (existing-unit! ix (file-unit-key! ix (wanted path) path) digest-of))
        link! (fn [path u]
                (let [{:keys [ord external?]} (wanted path)]
                  (when-not (= (current path) {:unit-id u :external? external? :ord ord})
                    (snapshot/set-file-unit! w p path u {:ord ord :external? external?}))))]
    (db/execute! c "UPDATE project SET config_hash = ?, last_seen = ? WHERE id = ?"
                 (:hash project-config) (System/currentTimeMillis) p)
    (link-jars! ix p ctx)
    ;; files: forget vanished ones, link unchanged ones, queue the rest
    (doseq [batch (partition-all files-per-tx (remove wanted (keys current)))]
      (writer/with-write-tx w (run! #(snapshot/remove-file! w p %) batch)))
    (let [missing (into [] (mapcat (fn [batch]
                                     (writer/with-write-tx w
                                       (reduce (fn [missing path]
                                                 (if-let [u (unit-of nil path)]
                                                   (do (link! path u) missing)
                                                   (conj missing path)))
                                               [] batch))))
                        (partition-all files-per-tx (keys wanted)))
          ;; a file whose hooks asked about other namespaces can only be
          ;; checked once everything else is linked
          digest-of (digest-fn ix p)]
      (doseq [batch (partition-all files-per-tx missing)]
        (writer/with-write-tx w
          (doseq [path batch
                  :let [u (unit-of digest-of path)]]
            (link! path u)
            (when-not u (queue/enqueue! c p :file path 1))))))))

(defn- files-job
  "Index project or external-dir files that changed (`batch`, of project
  `p`). Returns a job: {:analyze (fn [], the clj-kondo work) :write (fn
  [analysis])}."
  [{:keys [c w shards] :as ix} p batch]
  (let [ctx (context ix p)
        digest-of (digest-fn ix p)
        placed (mapv (fn [{:keys [path]}]
                       (let [f (io/file path)
                             canonical (.getCanonicalPath f)]
                         (if (.isFile f)
                           (assoc (file-placement ctx canonical) :path canonical)
                           {:path canonical :gone? true})))
                     batch)
        ;; files outside the project's dirs (:mode nil) are not part of it
        groups (vec (for [[[mode config] group] (group-by (juxt :mode :config) (filter :mode placed))]
                      {:mode mode :config config :group group
                       :known (into {} (map (fn [{:keys [path] :as placement}]
                                              [path (existing-unit! ix (file-unit-key! ix placement path) digest-of)]))
                                    group)}))
        serve (serve-fn ix p)]
    {:analyze (fn []
                (mapv (fn [{:keys [mode config known]}]
                        (analyze/analyze-files (into [] (comp (remove val) (map key)) known)
                                               {:config config :mode mode :shards shards :serve serve}))
                      groups))
     :write (fn [analysis]
              (doseq [{:keys [path gone?]} placed :when gone?]
                (snapshot/remove-file! w p path))
              (doseq [[{:keys [group known]} results] (map vector groups analysis)]
                (let [;; no :unit-key: changed while analyzed
                      [analyzed changed] ((juxt filter remove) :unit-key results)
                      ids (merge (into {} (filter val) known)
                                 (zipmap (map :path analyzed)
                                         (writer/write-units! w (map (juxt :unit-key :elements) analyzed))))
                      changed-paths (set (map :path changed))]
                  (writer/with-write-tx w
                    (doseq [{:keys [path ord external?]} group
                            :when (not (changed-paths path))]
                      (snapshot/set-file-unit! w p path (ids path) {:ord ord :external? external?})))
                  ;; changed while analyzed: again. Unreadable: left out,
                  ;; until it changes (it would be queued forever)
                  (doseq [path changed-paths]
                    (if (.canRead (io/file path))
                      (queue/enqueue! c p :file path 1)
                      (log/warn "can't read" path "- skipped")))))
              (recheck-dependents! ix p (into {} (map (juxt :path :enqueued-at)) batch)))}))

(defn- delete-files! [{:keys [c w] :as ix} p paths]
  (let [current (keys (snapshot/file-paths c p))]
    (writer/with-write-tx w
      (doseq [path paths
              :let [path (.getCanonicalPath (io/file path))]
              ;; a directory: the files under it
              gone (cons path (filter #(under? path %) current))]
        (snapshot/remove-file! w p gone))))
  (recheck-dependents! ix p))

(defn- jars-job
  "Analyze jars (those of `batch` not indexed yet) and link them into
  project `p`. Returns a job: {:analyze (fn [], the clj-kondo work) :write
  (fn [analysis])}."
  [{:keys [c w home shards] :as ix} p batch]
  (let [{:keys [jars jar-context]} (context ix p)
        ord-of (into {} (map (fn [[ord path]] [path ord])) jars)
        todo (vec (for [{:keys [path]} batch
                        :let [ord (ord-of path)]
                        ;; a jar no longer on the classpath: nothing to do
                        :when ord
                        :let [config (kc/jar-config! home jar-context path)
                              h (fingerprint/content-hash! c path)]]
                    {:path path :ord ord :config config :hash h
                     :jar-id (snapshot/jar-id c (analyze/jar-key h config))}))]
    {:analyze (fn []
                (analyze/analyze-jars (mapv :path (remove :jar-id todo))
                                      {:configs (into {} (map (juxt :path :config)) todo)
                                       :jar-hashes (into {} (map (juxt :path :hash)) todo)
                                       :shards shards}))
     :write (fn [analyzed]
              (let [written (into {} (for [{:keys [jar jar-key entries]} analyzed]
                                       [jar (snapshot/write-jar! w jar-key (map (juxt :entry-path :unit-key :elements) entries))]))]
                (doseq [{:keys [path ord jar-id]} todo]
                  (snapshot/link-jar! w p ord (or jar-id (written path))))
                (recheck-dependents! ix p)))}))

(defn- index-dep-files!
  [{:keys [c w home shards] :as ix} p paths]
  (let [{:keys [jar-context]} (context ix p)]
    (doseq [path paths
            :let [jar-hash (some-> (sources/source-of home path) :jar-hash-hex digest/unhex)
                  jar-path (when jar-hash
                             (db/query-value c "SELECT pj.path FROM project_jar pj JOIN jar j ON j.id = pj.jar_id
                                                WHERE pj.project_id = ? AND j.jar_hash = ?" p jar-hash))]
            :when (and jar-path (.isFile (io/file path)))]
      (let [config (kc/jar-config! home jar-context jar-path)
            [{:keys [unit-key elements]}] (analyze/analyze-files [path] {:config config :mode :dep-file :shards shards})]
        ;; changed while analyzed (re-extracted): next time it's opened
        (when unit-key
          (let [[u] (writer/write-units! w [[unit-key elements]])]
            (snapshot/set-dep-file-unit! w path jar-hash u)))))))

(def ^:private environment-codes
  "Primary SQLite result codes of the machine failing: a full disk, an I/O
  error (extended codes, like SQLITE_IOERR_WRITE, share their primary
  code's low byte)."
  #{(.code SQLiteErrorCode/SQLITE_FULL) (.code SQLiteErrorCode/SQLITE_IOERR)})

(defn- environment-failure?
  "Did the machine fail, rather than the input? Retrying later can succeed."
  [e]
  (some #(and (instance? SQLiteException %)
              (contains? environment-codes (bit-and 0xff (.code (.getResultCode ^SQLiteException %)))))
        (take-while some? (iterate ex-cause e))))

(defn- failed!
  "A batch failed with `e`. A machine failure keeps it queued (:retry); bad
  input is dropped, so it can't stop the daemon or be retried forever."
  [{:keys [c]} batch e]
  (let [{:keys [project-id kind]} (first batch)
        warn #(log/warn % kind (count batch) "item(s) of project" project-id ":" (ex-message e))]
    (if (environment-failure? e)
      (do (warn "will retry") :retry)
      (do (warn "dropped")
          (queue/done! c batch)
          batch))))

(def ^:private pipelined-kinds #{:file :jar})

(defn- start!
  "Prepare a :file or :jar batch and start its analysis, once the analysis
  of the batch before (`previous`, in flight) is done."
  [ix {:keys [project-id kind]} batch previous]
  (let [job (try ((case kind :file files-job :jar jars-job) ix project-id batch)
                 ;; Throwable: an Error too must drop its batch (`finish!`)
                 (catch Throwable e {:error e}))
        before (get-in previous [:job :analysis])]
    {:batch batch
     :job (cond-> job
            (:analyze job) (assoc :analysis (future
                                              (when before (try @before (catch Throwable _ nil)))
                                              ((:analyze job)))))}))

(defn- finish!
  "Write a started batch once its analysis is done. Returns the batch, or
  :retry."
  [{:keys [c] :as ix} {:keys [batch job]}]
  (try
    (when-let [e (:error job)] (throw e))
    ((:write job) @(:analysis job))
    (queue/done! c batch)
    batch
    (catch ExecutionException e (failed! ix batch (or (ex-cause e) e)))
    ;; Throwable: an Error (out of memory) must drop its batch too, not end
    ;; the daemon and leave the batch first in line for the next
    (catch Throwable e (failed! ix batch e))))

(defn- run-batch!
  "Process a batch of a kind that isn't pipelined. Returns the batch, or
  :retry."
  [{:keys [c] :as ix} batch]
  (let [{:keys [project-id kind]} (first batch)
        paths (mapv :path batch)]
    (try
      (case kind
        :sync (sync-project! ix project-id)
        :delete (delete-files! ix project-id paths)
        :dep-file (index-dep-files! ix project-id paths))
      (queue/done! c batch)
      batch
      (catch Throwable e (failed! ix batch e)))))

(defn step!
  "Advance the queue by one batch. Returns something truthy while there is
  work (:retry when the machine failed, a full disk: the batch stays
  queued, to try again later), nil when the queue is empty and nothing is
  in flight.

  :file and :jar batches are pipelined: the next one is prepared and its
  analysis started before the one in flight is written."
  [{:keys [c batch-sizes in-flight] :as ix}]
  (let [current @in-flight
        batch (queue/next-batch c batch-sizes (map queue/request-key (:batch current)))
        head (first batch)
        started (when (and head (pipelined-kinds (:kind head))) (start! ix head batch current))
        finished (when current (finish! ix current))]
    (cond
      (= :retry finished)
      ;; the batch just started stays in flight, its analysis running: it's
      ;; written on the next try (cancelling it would stop only its outer
      ;; task, leaving its clj-kondo runs going beside the next analysis)
      (do (reset! in-flight started)
          :retry)

      started
      (do (reset! in-flight started) (or finished (:batch started)))

      head
      (do (reset! in-flight nil) (run-batch! ix batch))

      :else
      (do (reset! in-flight nil) finished))))

(defn drain!
  "Write the batch in flight, if any, without starting another."
  [{:keys [in-flight] :as ix}]
  (when-let [current @in-flight]
    (reset! in-flight nil)
    (finish! ix current)))

(defn run-until-idle!
  "Process batches until the queue is empty and nothing is in flight (nil),
  or the machine fails (:retry)."
  [ix]
  (loop []
    (when-let [r (step! ix)]
      (if (= :retry r) :retry (recur)))))
