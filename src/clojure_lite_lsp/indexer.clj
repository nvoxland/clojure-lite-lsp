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
  clj-kondo's process-wide state, and batches can have different configs. Every database access (preparing a batch,
  writing one) still happens on the loop thread, one after the other:
  only analysis, which touches no database, runs alongside."
  (:require
   [clojure-lite-lsp.analyze :as analyze]
   [clojure-lite-lsp.classpath :as classpath]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.fingerprint :as fingerprint]
   [clojure-lite-lsp.kondo-config :as kc]
   [clojure-lite-lsp.ns-analysis :as nsa]
   [clojure-lite-lsp.queue :as queue]
   [clojure-lite-lsp.reuse :as reuse]
   [clojure-lite-lsp.snapshot :as snapshot]
   [clojure-lite-lsp.sources :as sources]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.io Closeable File]))

(set! *warn-on-reflection* true)

(defrecord Indexer [c w reader cache-dir shards contexts batch-sizes in-flight reuser requeued]
  Closeable
  (close [_]
    (when (realized? reader) (.close ^java.sql.Connection @reader))
    (.close ^java.sql.Connection c)))

(defn default-cache-dir []
  (io/file (System/getProperty "user.home") ".cache" "clojure-lite-lsp"))

(defn indexer
  "An indexer writing to the index at `db-path`, keeping clj-kondo configs
  under `cache-dir`."
  [{:keys [db-path cache-dir shards batch-sizes] :or {shards 8}}]
  (let [c (db/open-writer db-path)]
    (map->Indexer {:c c :w (writer/writer c)
                   ;; for analysis, which runs beside the loop thread's writes
                   :reader (delay (db/open-reader db-path))
                   :cache-dir (or cache-dir (default-cache-dir))
                   :shards shards
                   :batch-sizes batch-sizes
                   :contexts (atom {})
                   ;; the batch being analyzed: {:batch :job}
                   :in-flight (atom nil)
                   :reuser (reuse/reuser (or cache-dir (default-cache-dir)))
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

(defn- compute-context [{:keys [c cache-dir]} p]
  (let [root (root-of c p)
        entries (classpath/memoized! c p root)
        by-kind (group-by :kind entries)
        jars (mapv :path (:jar by-kind))]
    {:root root
     :source-dirs (mapv :path (:source-dir by-kind))
     :external-dirs (mapv (fn [{:keys [path ord]}] {:path path :ord ord :config (kc/dir-config! cache-dir path)})
                          (:external-dir by-kind))
     :jars (mapv (juxt :ord :path) (:jar by-kind))
     :project-config (kc/project-config! cache-dir root entries)
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
  (cond
    (some #(under? % path) source-dirs)
    {:ord 0 :external? false :mode :project :config project-config}

    :else
    (or (when-let [{:keys [ord config]} (first (filter #(under? (:path %) path) external-dirs))]
          {:ord ord :external? true :mode :dependency :config config})
        ;; anywhere else under the root (build.clj, scripts/): when asked
        ;; for (an editor opened it), as the project's own
        (when (under? root path)
          {:ord 0 :external? false :mode :project :config project-config}))))

(defn- file-unit-key [{:keys [c]} {:keys [mode config]} path]
  (when-let [h (fingerprint/content-hash! c path)]
    (analyze/unit-key mode config h path)))

(defn- serve-fn
  "Answers for hooks asking about a namespace (clojure-lite-lsp.ns-analysis), as project
  `p` sees it: for analysis, off the loop thread."
  [{:keys [reader]} p]
  (fn [lang ns-sym]
    (let [r @reader]
      (locking r (nsa/index-answer r p lang ns-sym)))))

(defn- digest-fn
  "Digests of today's answers in project `p`, on the loop thread."
  [{:keys [c]} p]
  (memoize (fn [lang ns-sym]
             (nsa/digest (analyze/answer (fn [l n] (nsa/index-answer c p l n)) lang ns-sym)))))

(defn- existing-unit
  "The unit for a file's key: analyzed under this config, or under another
  whose differences don't touch what the file references (clojure-lite-lsp.reuse), and
  whose hooks' answers `digest-of` finds still hold."
  [{:keys [w reuser]} unit-key digest-of]
  (when unit-key (reuse/unit-for reuser w unit-key digest-of)))

(def ^:private requeues-remembered
  "How many (unit, answers) pairs of a file the requeue guard remembers."
  8)

(defn requeue?
  "Should a file (`k`) whose analysis doesn't match its hooks' answers be
  queued again, for `entry` [unit answers]? Once per entry: had analyzing
  it given back one already seen, it would cycle forever. `seen` is an
  atom {k [entry ...]}."
  [seen k entry]
  (if (some #{entry} (@seen k))
    false
    (do (swap! seen update k #(vec (take-last requeues-remembered (conj (or % []) entry))))
        true)))

(defn- recheck-dependents!
  "Queue the files of project `p` whose hooks' answers no longer hold.

  A file is queued again for the same unit and the same answers only once:
  had analyzing it again given back that unit, nothing would change, and it
  would be queued forever."
  [{:keys [c requeued in-flight] :as ix} p]
  (when-let [marker (db/query-value c "SELECT id FROM sym WHERE text = ?" nsa/marker)]
    (let [digest-of (digest-fn ix p)]
      (doseq [[u] (db/query c "SELECT r.unit_id FROM unit_ref r
                               JOIN project_unit pu ON pu.unit_id = r.unit_id AND pu.project_id = ?
                               WHERE r.ref = ?" p marker)
              :let [deps (reuse/ns-deps c u)
                    today (mapv (fn [[lang ns-sym]] (digest-of lang ns-sym)) deps)]
              :when (not= (map last deps) today)
              [path] (db/query c "SELECT path FROM project_file WHERE project_id = ? AND unit_id = ?" p u)]
        (if (requeue? requeued [p path] [u today])
          (queue/enqueue! c p :file path 1)
          ;; unless it's still waiting to be analyzed again, that didn't help
          (when-not (or (db/query-value c "SELECT 1 FROM pending WHERE project_id = ? AND kind = 'file' AND path = ?" p path)
                        (some #(= path (:path %)) (:batch @in-flight)))
            (binding [*out* *err*]
              (println "clojure-lite-lsp: analysis of" path "doesn't match what its hooks are told; left as is"))))))))

;;;; work

(def ^:private files-per-tx 500)

(defn- sync-project!
  [{:keys [c w cache-dir contexts] :as ix} p]
  (swap! contexts dissoc p)
  (let [{:keys [source-dirs external-dirs jars jar-context project-config] :as ctx} (context ix p)]
    (db/execute! c "UPDATE project SET config_hash = ?, last_seen = ? WHERE id = ?"
                 (:hash project-config) (System/currentTimeMillis) p)
    ;; jars: link the ones already indexed, enqueue the rest
    (let [linked (vec (for [[ord path] jars
                            :let [config (kc/jar-config! cache-dir jar-context path)
                                  h (fingerprint/content-hash! c path)]]
                        [ord path (snapshot/jar-id c (analyze/jar-key h config))]))]
      (snapshot/set-project-jars! w p linked)
      (doseq [[_ path jar-id] linked :when (nil? jar-id)]
        (queue/enqueue! c p :jar path 2)))
    ;; files: link unchanged ones, enqueue the rest, forget vanished ones
    (let [current (snapshot/file-paths c p)
          ;; a file outside the source dirs, indexed because an editor
          ;; opened it: kept (and checked) while it exists
          kept? (fn [path] (and (under? (:root ctx) path) (.isFile (io/file path))
                                (re-find source-extensions path)))
          listed (into {} (for [dir (concat source-dirs (map :path external-dirs))
                                path (source-files dir)]
                            [path (file-placement ctx path)]))
          wanted (into listed (for [path (keys current)
                                    :when (and (not (listed path)) (kept? path))]
                                [path (file-placement ctx path)]))]
      (let [missing (atom [])
            link! (fn [path {:keys [ord external?]} u]
                    (when-not (= (current path) {:unit-id u :external? external? :ord ord})
                      (snapshot/set-file-unit! w p path u {:ord ord :external? external?})))]
        (doseq [batch (partition-all files-per-tx (concat (keys wanted) (remove wanted (keys current))))]
          (writer/with-write-tx w
            (doseq [path batch]
              (if-let [placement (wanted path)]
                (if-let [u (existing-unit ix (file-unit-key ix placement path) nil)]
                  (link! path placement u)
                  (swap! missing conj path))
                (snapshot/remove-file! w p path)))))
        ;; a file whose hooks asked about other namespaces can only be
        ;; checked once everything else is linked
        (let [digest-of (digest-fn ix p)]
          (doseq [batch (partition-all files-per-tx @missing)]
            (writer/with-write-tx w
              (doseq [path batch
                      :let [placement (wanted path)
                            u (existing-unit ix (file-unit-key ix placement path) digest-of)]]
                (link! path placement u)
                (when-not u (queue/enqueue! c p :file path 1))))))))))

(defn- files-job
  "Index project or external-dir files that changed. Returns a job:
  {:analyze (fn [], the clj-kondo work) :write (fn [analysis])}."
  [{:keys [c w shards] :as ix} p paths]
  (let [ctx (context ix p)
        digest-of (digest-fn ix p)
        placed (for [path paths
                     :let [f (io/file path)]]
                 (if (.isFile f)
                   (assoc (file-placement ctx (.getCanonicalPath f)) :path (.getCanonicalPath f))
                   {:path (.getCanonicalPath f) :gone? true}))
        ;; files outside the project's dirs (:mode nil) are not part of it
        groups (vec (for [[[mode config] group] (group-by (juxt :mode :config) (filter :mode placed))
                          :let [placement (first group)]]
                      {:mode mode :config config :group group
                       :known (into {} (for [{:keys [path]} group]
                                         [path (existing-unit ix (file-unit-key ix placement path) digest-of)]))}))
        serve (serve-fn ix p)]
    {:analyze (fn []
                (mapv (fn [{:keys [mode config known]}]
                        (analyze/analyze-files (vec (keep (fn [[path u]] (when-not u path)) known))
                                               {:config config :mode mode :shards shards :serve serve}))
                      groups))
     :write (fn [analysis]
              (doseq [{:keys [path gone?]} placed :when gone?]
                (snapshot/remove-file! w p path))
              (doseq [[{:keys [group known]} fresh] (map vector groups analysis)]
                (let [;; changed while analyzed: analyze it again
                      [fresh changed] ((juxt filter remove) :unit-key fresh)
                      fresh-ids (writer/write-units! w (map (juxt :unit-key :elements) fresh))
                      ids (merge (into {} (filter second) known)
                                 (zipmap (map :path fresh) fresh-ids))
                      changed (set (map :path changed))]
                  (writer/with-write-tx w
                    (doseq [{:keys [path ord external?]} group
                            :when (not (changed path))]
                      (snapshot/set-file-unit! w p path (ids path) {:ord ord :external? external?})))
                  ;; changed while analyzed: again. Unreadable: left out,
                  ;; until it changes (it would be queued forever)
                  (doseq [path changed]
                    (if (.canRead (io/file path))
                      (queue/enqueue! c p :file path 1)
                      (binding [*out* *err*] (println "clojure-lite-lsp: can't read" path "- skipped"))))))
              (recheck-dependents! ix p))}))

(defn- delete-files! [{:keys [c w] :as ix} p paths]
  (let [current (keys (snapshot/file-paths c p))]
    (doseq [path paths
            :let [path (.getCanonicalPath (io/file path))
                  ;; a directory: the files under it
                  under (filter #(str/starts-with? % (str path File/separator)) current)]
            gone (cons path under)]
      (snapshot/remove-file! w p gone)))
  (recheck-dependents! ix p))

(defn- jars-job
  "Analyze jars (those not indexed yet) and link them into the project.
  Returns a job: {:analysis (a future) :write (fn [analysis])}."
  [{:keys [c w cache-dir shards] :as ix} p paths]
  (let [{:keys [jars jar-context]} (context ix p)
        ord-of (into {} (map (fn [[ord path]] [path ord])) jars)
        todo (vec (for [path paths
                        :let [ord (ord-of path)]
                        ;; a jar no longer on the classpath: nothing to do
                        :when ord
                        :let [config (kc/jar-config! cache-dir jar-context path)
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
  [{:keys [c w cache-dir shards] :as ix} p paths]
  (let [{:keys [jar-context]} (context ix p)]
    (doseq [path paths
            :let [jar-hash (some-> (sources/source-of cache-dir path) :jar-hash-hex sources/unhex)
                  jar-path (when jar-hash
                             (db/query-value c "SELECT pj.path FROM project_jar pj JOIN jar j ON j.id = pj.jar_id
                                                WHERE pj.project_id = ? AND j.jar_hash = ?" p jar-hash))]
            :when (and jar-path (.isFile (io/file path)))]
      (let [config (kc/jar-config! cache-dir jar-context jar-path)
            [{:keys [unit-key elements]}] (analyze/analyze-files [path] {:config config :mode :dep-file :shards shards})]
        ;; changed while analyzed (re-extracted): next time it's opened
        (when unit-key
          (let [[u] (writer/write-units! w [[unit-key elements]])]
            (snapshot/set-dep-file-unit! w path jar-hash u)))))))

(defn- environment-failure?
  "Did the machine fail, rather than the input: a full disk or an I/O
  error? Retrying later can succeed."
  [^Throwable e]
  (some (fn [^Throwable t]
          (when (instance? org.sqlite.SQLiteException t)
            (contains? #{org.sqlite.SQLiteErrorCode/SQLITE_FULL org.sqlite.SQLiteErrorCode/SQLITE_IOERR}
                       (.getResultCode ^org.sqlite.SQLiteException t))))
        (take-while some? (iterate #(.getCause ^Throwable %) e))))

(defn- failed!
  "A batch failed with `e`. A machine failure keeps it queued (:retry); bad
  input is dropped, so it can't stop the daemon or be retried forever."
  [{:keys [c]} batch ^Throwable e]
  (let [{:keys [project-id kind]} (first batch)
        log #(binding [*out* *err*]
               (println "clojure-lite-lsp:" % kind (count batch) "item(s) of project" project-id ":" (ex-message e)))]
    (if (environment-failure? e)
      (do (log "will retry") :retry)
      (do (log "dropped")
          (queue/done! c batch)
          batch))))

(def ^:private pipelined-kinds #{:file :jar})

(defn- start!
  "Prepare a :file or :jar batch and start its analysis, once the analysis
  of the batch before (`previous`, in flight) is done."
  [ix {:keys [project-id kind]} batch previous]
  (let [paths (mapv :path batch)
        job (try ((case kind :file files-job :jar jars-job) ix project-id paths)
                 (catch Exception e {:error e}))
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
    (catch java.util.concurrent.ExecutionException e (failed! ix batch (or (.getCause e) e)))
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
      ;; the batch just started is abandoned too: its rows stay queued
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
    (let [r (step! ix)]
      (cond (= :retry r) :retry
            r (recur)))))
