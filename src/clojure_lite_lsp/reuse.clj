(ns clojure-lite-lsp.reuse
  "Reusing a file's analysis across clj-kondo configs.

  A unit's key covers its config, so any change to a project's config
  (a branch that edits .clj-kondo) would otherwise re-analyze every file.
  But a file's analysis only depends on the config for what the file
  references (clojure-lite-lsp.config-sig): a unit analyzed under config K1 is valid
  under K2 when their global parts agree and no symbol the unit references
  (unit_ref) has a different entry. Such a unit is found by its base key
  (the same file, clj-kondo and options) and given K2's key as an alias,
  so the next lookup is direct.

  A unit whose hooks asked about other namespaces (clojure-lite-lsp.ns-analysis) is
  valid only where the answers are still the same. It has its own key
  (the answers are part of it), is found only this way, and never gets
  an alias: whether it holds depends on the project."
  (:require
   [clojure-lite-lsp.config-sig :as config-sig]
   [clojure-lite-lsp.db :as db]
   [clojure-lite-lsp.digest :as digest]
   [clojure-lite-lsp.ns-analysis :as nsa]
   [clojure-lite-lsp.writer :as writer]
   [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(defn reuser
  "Reuse state for a daemon: signatures of the materialized config dirs
  under `cache-dir`, and their pairwise diffs, memoized."
  [cache-dir]
  {:cache-dir cache-dir :signatures (atom {}) :diffs (atom {})})

(defn- cached!
  "The value of `k` in atom `cache`, computing (f) and storing it the first
  time (nil included)."
  [cache k f]
  (if-let [[_ v] (find @cache k)]
    v
    (let [v (f)]
      (swap! cache assoc k v)
      v)))

(defn- signature
  "The signature of the config with `config-hash`, or nil when its dir is
  gone (a cleared cache: then nothing is reused)."
  [{:keys [cache-dir signatures]} config-hash]
  (let [k (digest/hex config-hash)]
    (cached! signatures k
             #(let [dir (io/file cache-dir "configs" k)]
                (when (.isDirectory dir) (config-sig/signature (str dir)))))))

(defn- config-diff [{:keys [diffs] :as r} from-hash to-hash]
  (cached! diffs [(digest/hex from-hash) (digest/hex to-hash)]
           #(let [from (signature r from-hash)
                  to (signature r to-hash)]
              (if (and from to) (config-sig/diff from to) {:global-same? false}))))

(defn- references-any?
  "Does unit `u` reference any of `refs` (a set of \"ns/name\" or
  \"ns:name\")? One query for the unit's refs, however many changed."
  [c u refs]
  (when (seq refs)
    (some refs (writer/unit-refs c u))))

(defn ns-deps
  "The answers unit `u`'s hooks got: [[lang ns-sym digest]]."
  [c u]
  (keep (comp nsa/parse-ref first)
        (db/query c "SELECT s.text FROM unit_ref r JOIN sym s ON s.id = r.ref
                     WHERE r.unit_id = ? AND s.text >= 'nsa:' AND s.text < 'nsa;'" u)))

(defn- hold?
  "Would hooks get the answers `deps` recorded today? `digest-of` (fn [lang
  ns-sym]) gives today's; nil means they can't be checked."
  [deps digest-of]
  (or (empty? deps)
      (and digest-of
           (every? (fn [[lang ns-sym d]] (= d (digest-of lang ns-sym))) deps))))

(def ^:private candidates-to-try
  "How many analyses of the same file under other configs to consider."
  5)

(defn- reusable-deps
  "When unit `u`, analyzed under config `from`, is as valid under config
  `to`: its hooks' answers (`ns-deps`), else nil."
  [r c u from to digest-of]
  (let [{:keys [global-same? changed custom-readers]} (config-diff r from to)]
    (when (and global-same?
               ;; a changed custom key reaches analysis only through the
               ;; hooks that read it: files calling their macros
               (not (references-any? c u (into changed custom-readers))))
      (let [deps (ns-deps c u)]
        (when (hold? deps digest-of) deps)))))

(defn unit-for
  "The unit for `unit-key`: the one with that key, else one analyzed under
  another config that is equally valid under this one (recorded as an alias
  through writer `w`). nil when the file needs analyzing. `digest-of`
  checks units whose hooks asked about namespaces (`hold?`)."
  [r {:keys [c] :as w} unit-key digest-of]
  (or (writer/unit-id c unit-key)
      ;; an external dir's file is analyzed without usages, so it records
      ;; no references: nothing shows which configs it's safe under
      (when-not (:external? unit-key)
        (some (fn [[u other-config]]
                (when-let [deps (reusable-deps r c u other-config (:config-hash unit-key) digest-of)]
                  ;; with answers, whether it holds depends on the project: no alias
                  (when (empty? deps) (writer/add-unit-key! w unit-key u))
                  u))
              (take candidates-to-try (writer/units-with-base c unit-key))))))
