(ns csl.reuse
  "Reusing a file's analysis across clj-kondo configs.

  A unit's key covers its config, so any change to a project's config
  (a branch that edits .clj-kondo) would otherwise re-analyze every file.
  But a file's analysis only depends on the config for what the file
  references (csl.config-sig): a unit analyzed under config K1 is valid
  under K2 when their global parts agree and no symbol the unit references
  (unit_ref) has a different entry. Such a unit is found by its base key
  (the same file, clj-kondo and options) and given K2's key as an alias,
  so the next lookup is direct."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [csl.config-sig :as config-sig]
   [csl.db :as db]
   [csl.writer :as writer]))

(set! *warn-on-reflection* true)

(defn reuser
  "Reuse state for a daemon: signatures of the materialized config dirs
  under `cache-dir`, and their pairwise diffs, memoized."
  [cache-dir]
  {:cache-dir cache-dir :signatures (atom {}) :diffs (atom {})})

(defn- hex [^bytes bs] (apply str (map #(format "%02x" %) bs)))

(defn- signature
  "The signature of the config with `config-hash`, or nil when its dir is
  gone (a cleared cache: then nothing is reused)."
  [{:keys [cache-dir signatures]} config-hash]
  (let [k (hex config-hash)]
    (if (contains? @signatures k)
      (@signatures k)
      (let [dir (io/file cache-dir "configs" k)
            s (when (.isDirectory dir) (config-sig/signature (str dir)))]
        (swap! signatures assoc k s)
        s))))

(defn- config-diff [{:keys [diffs] :as r} from-hash to-hash]
  (let [k [(hex from-hash) (hex to-hash)]]
    (or (@diffs k)
        (let [from (signature r from-hash)
              to (signature r to-hash)
              d (if (and from to) (config-sig/diff from to) {:global-same? false})]
          (swap! diffs assoc k d)
          d))))

(defn- references-any?
  "Does unit `u` reference any of `refs` (\"ns/name\" or \"ns:name\")?"
  [c u refs]
  (when (seq refs)
    (let [ids (keep #(db/query-value c "SELECT id FROM sym WHERE text = ?" %) refs)]
      (and (seq ids)
           (some? (apply db/query-value c (str "SELECT 1 FROM unit_ref WHERE unit_id = ? AND ref IN ("
                                               (str/join "," (repeat (count ids) "?")) ") LIMIT 1")
                         u ids))))))

(def ^:private candidates-to-try
  "How many analyses of the same file under other configs to consider."
  5)

(defn unit-for
  "The unit for `unit-key`: the one with that key, else one analyzed under
  another config that is equally valid under this one (recorded as an alias
  through writer `w`). nil when the file needs analyzing."
  [r {:keys [c] :as w} unit-key]
  (or (writer/unit-id c unit-key)
      (some (fn [[u other-config]]
              (let [{:keys [global-same? changed]} (config-diff r other-config (:config-hash unit-key))]
                (when (and global-same? (not (references-any? c u changed)))
                  (writer/add-unit-key! w unit-key u)
                  u)))
            (take candidates-to-try (writer/units-with-base c unit-key)))))
