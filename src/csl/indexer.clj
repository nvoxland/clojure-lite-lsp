(ns csl.indexer
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
            csl.sources)

  Analysis is sharded across concurrent clj-kondo runs (csl.analyze), and
  pipelined: while one :file or :jar batch is written, the next one is
  already being analyzed. Every database access (preparing a batch,
  writing one) still happens on the loop thread, one after the other:
  only analysis, which touches no database, runs alongside."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.analyze :as analyze]
   [csl.classpath :as classpath]
   [csl.db :as db]
   [csl.fingerprint :as fingerprint]
   [csl.kondo-config :as kc]
   [csl.queue :as queue]
   [csl.reuse :as reuse]
   [csl.snapshot :as snapshot]
   [csl.sources :as sources]
   [csl.writer :as writer])
  (:import
   [java.io Closeable File]))

(set! *warn-on-reflection* true)

(defrecord Indexer [c w cache-dir shards contexts batch-sizes in-flight reuser]
  Closeable
  (close [_] (.close ^java.sql.Connection c)))

(defn default-cache-dir []
  (io/file (System/getProperty "user.home") ".cache" "clojure-sqlite-lsp"))

(defn indexer
  "An indexer writing to the index at `db-path`, keeping clj-kondo configs
  under `cache-dir`."
  [{:keys [db-path cache-dir shards batch-sizes] :or {shards 8}}]
  (let [c (db/open-writer db-path)]
    (map->Indexer {:c c :w (writer/writer c)
                   :cache-dir (or cache-dir (default-cache-dir))
                   :shards shards
                   :batch-sizes batch-sizes
                   :contexts (atom {})
                   ;; the batch being analyzed: {:batch :job}
                   :in-flight (atom nil)
                   :reuser (reuse/reuser (or cache-dir (default-cache-dir)))})))

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
  [{:keys [source-dirs external-dirs project-config]} path]
  (cond
    (some #(under? % path) source-dirs)
    {:ord 0 :external? false :mode :project :config project-config}

    :else
    (when-let [{:keys [ord config]} (first (filter #(under? (:path %) path) external-dirs))]
      {:ord ord :external? true :mode :dependency :config config})))

(defn- file-unit-key [{:keys [c]} {:keys [mode config]} path]
  (when-let [h (fingerprint/content-hash! c path)]
    (analyze/unit-key mode config h path)))

(defn- existing-unit
  "The unit for a file's key: analyzed under this config, or under another
  whose differences don't touch what the file references (csl.reuse)."
  [{:keys [w reuser]} unit-key]
  (when unit-key (reuse/unit-for reuser w unit-key)))

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
          wanted (into {} (for [dir (concat source-dirs (map :path external-dirs))
                                path (source-files dir)]
                            [path (file-placement ctx path)]))]
      (doseq [batch (partition-all files-per-tx (concat (keys wanted) (remove wanted (keys current))))]
        (writer/with-write-tx w
          (doseq [path batch]
            (if-let [{:keys [ord external?] :as placement} (wanted path)]
              (let [u (existing-unit ix (file-unit-key ix placement path))
                    before (current path)]
                (when-not (= before {:unit-id u :external? external? :ord ord})
                  (snapshot/set-file-unit! w p path u {:ord ord :external? external?}))
                (when-not u (queue/enqueue! c p :file path 1)))
              (snapshot/remove-file! w p path))))))))

(defn- files-job
  "Index project or external-dir files that changed. Returns a job:
  {:analysis (a future of the clj-kondo work) :write (fn [analysis])}."
  [{:keys [c w shards] :as ix} p paths]
  (let [ctx (context ix p)
        placed (for [path paths
                     :let [f (io/file path)]]
                 (if (.isFile f)
                   (assoc (file-placement ctx (.getCanonicalPath f)) :path (.getCanonicalPath f))
                   {:path path :gone? true}))
        ;; files outside the project's dirs (:mode nil) are not part of it
        groups (vec (for [[[mode config] group] (group-by (juxt :mode :config) (filter :mode placed))
                          :let [placement (first group)]]
                      {:mode mode :config config :group group
                       :known (into {} (for [{:keys [path]} group]
                                         [path (existing-unit ix (file-unit-key ix placement path))]))}))]
    {:analysis (future
                 (mapv (fn [{:keys [mode config known]}]
                         (analyze/analyze-files (vec (keep (fn [[path u]] (when-not u path)) known))
                                                {:config config :mode mode :shards shards}))
                       groups))
     :write (fn [analysis]
              (doseq [{:keys [path gone?]} placed :when gone?]
                (snapshot/remove-file! w p path))
              (doseq [[{:keys [group known]} fresh] (map vector groups analysis)]
                (let [fresh-ids (writer/write-units! w (map (juxt :unit-key :elements) fresh))
                      ids (merge (into {} (filter second) known)
                                 (zipmap (map :path fresh) fresh-ids))]
                  (writer/with-write-tx w
                    (doseq [{:keys [path ord external?]} group]
                      (snapshot/set-file-unit! w p path (ids path) {:ord ord :external? external?}))))))}))

(defn- delete-files! [{:keys [w]} p paths]
  (doseq [path paths] (snapshot/remove-file! w p path)))

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
    {:analysis (future
                 (analyze/analyze-jars (mapv :path (remove :jar-id todo))
                                       {:configs (into {} (map (juxt :path :config)) todo)
                                        :jar-hashes (into {} (map (juxt :path :hash)) todo)
                                        :shards shards}))
     :write (fn [analyzed]
              (let [written (into {} (for [{:keys [jar jar-key entries]} analyzed]
                                       [jar (snapshot/write-jar! w jar-key (map (juxt :entry-path :unit-key :elements) entries))]))]
                (doseq [{:keys [path ord jar-id]} todo]
                  (snapshot/link-jar! w p ord (or jar-id (written path))))))}))

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
            [{:keys [unit-key elements]}] (analyze/analyze-files [path] {:config config :mode :dep-file :shards shards})
            [u] (writer/write-units! w [[unit-key elements]])]
        (snapshot/set-dep-file-unit! w path jar-hash u)))))

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
               (println "csl:" % kind (count batch) "item(s) of project" project-id ":" (ex-message e)))]
    (if (environment-failure? e)
      (do (log "will retry") :retry)
      (do (log "dropped")
          (queue/done! c batch)
          batch))))

(def ^:private pipelined-kinds #{:file :jar})

(defn- start!
  "Prepare a :file or :jar batch and start its analysis."
  [ix {:keys [project-id kind]} batch]
  (let [paths (mapv :path batch)]
    {:batch batch
     :job (try ((case kind :file files-job :jar jars-job) ix project-id paths)
               (catch Exception e {:error e}))}))

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
    (catch Exception e (failed! ix batch e))))

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
      (catch Exception e (failed! ix batch e)))))

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
        started (when (and head (pipelined-kinds (:kind head))) (start! ix head batch))
        finished (when current (finish! ix current))]
    (cond
      (= :retry finished)
      ;; the batch just started is abandoned too: its rows stay queued
      (do (reset! in-flight nil) :retry)

      started
      (do (reset! in-flight started) (or finished (:batch started)))

      head
      (do (reset! in-flight nil) (run-batch! ix batch))

      :else
      (do (reset! in-flight nil) finished))))

(defn run-until-idle!
  "Process batches until the queue is empty and nothing is in flight (nil),
  or the machine fails (:retry)."
  [ix]
  (loop []
    (let [r (step! ix)]
      (cond (= :retry r) :retry
            r (recur)))))
