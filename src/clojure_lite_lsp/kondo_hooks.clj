(ns clojure-lite-lsp.kondo-hooks
  "What clojure-lite-lsp changes in how clj-kondo runs hooks. clj-kondo has
  no API for these, so two of its vars are wrapped (`install!`), and its
  hook context is reset between configs (`use-config!`):

  - clj-kondo.hooks-api/ns-analysis*: a hook asking about a namespace is
    answered from the index (`*lookups*`), and the question is recorded:
    whether the hook changes the code can depend on the answer.
  - clj-kondo.impl.hooks/hook-fn: hook lookups are cached here and take
    turns. clj-kondo's own cache is emptied whenever it sees another config
    object (each concurrent run has its own), and loading a hook's code
    into its one process-wide interpreter from two runs at once can lose
    the hook's vars.

  Checked against the clj-kondo version in deps.edn."
  (:require
   [clj-kondo.hooks-api]
   [clj-kondo.impl.hooks :as impl.hooks]
   [clj-kondo.impl.utils :as impl.utils]
   [clojure-lite-lsp.ns-analysis :as nsa]))

(def ^:dynamic *lookups*
  "While analyzing: {:serve (fn [lang ns-sym]) :record atom}. Hooks asking
  about a namespace are answered by :serve, and [file lang ns digest] is
  recorded for each question."
  nil)

(defn- original
  "Var `v`'s value before clojure-lite-lsp wrapped it."
  [v]
  (or (::original (meta v)) @v))

(defn- wrap-var!
  "Replace var `v`'s value with (wrap original), keeping the original on
  the var so reloading this namespace wraps it afresh rather than twice."
  [v wrap]
  (let [o (original v)]
    (alter-meta! v assoc ::original o)
    (alter-var-root v (constantly (wrap o)))))

(defn answer
  "The answer to a hook asking about `ns-sym` for `lang`, given `serve`;
  without one, clj-kondo's own (only its built-in namespaces, having no
  cache)."
  [serve lang ns-sym]
  (or (when serve (serve lang ns-sym))
      ((original #'clj-kondo.hooks-api/ns-analysis*) lang ns-sym)))

(def ^:private hook-lock (Object.))

(def ^:private resolved-hooks
  "The hook (or nil) for each [ns-sym var-sym] under the config clj-kondo
  last loaded (`hooks-config`)."
  (atom {}))

(def ^:private hooks-config
  "The config dir whose hooks clj-kondo has loaded."
  (atom nil))

(defn- cached-hook-fn [hook-fn]
  (fn [ctx config ns-sym var-sym & more]
    (let [k [ns-sym var-sym]]
      (if-let [e (find @resolved-hooks k)]
        (val e)
        ;; lookups take turns; running the hooks doesn't
        (locking hook-lock
          (if-let [e (find @resolved-hooks k)]
            (val e)
            (let [h (apply hook-fn ctx config ns-sym var-sym more)]
              (swap! resolved-hooks assoc k h)
              h)))))))

(defn- answering-ns-analysis [ns-analysis*]
  (fn [lang ns-sym]
    (if-let [{:keys [serve record]} *lookups*]
      (let [r (answer serve lang ns-sym)]
        (swap! record conj [(:filename impl.utils/*ctx*) lang ns-sym (nsa/digest r)])
        r)
      (ns-analysis* lang ns-sym))))

(def ^:private installed
  (delay
    (wrap-var! #'impl.hooks/hook-fn cached-hook-fn)
    (wrap-var! #'clj-kondo.hooks-api/ns-analysis* answering-ns-analysis)))

(defn install!
  "Wrap clj-kondo's vars (once)."
  []
  @installed)

(defn use-config!
  "Make clj-kondo load hooks afresh when `config-dir` isn't the one it last
  ran with. It loads a hook namespace once per process and reloads only a
  file it saw change, and every config is a directory of its own: an
  edited hook would otherwise keep its old code. Runs never overlap across
  configs (clojure-lite-lsp.indexer), so this can't pull hooks from under
  one."
  [config-dir]
  (when (not= config-dir @hooks-config)
    (locking hook-lock
      (impl.hooks/reset-ctx!)
      (reset! resolved-hooks {})
      (reset! hooks-config config-dir))))
