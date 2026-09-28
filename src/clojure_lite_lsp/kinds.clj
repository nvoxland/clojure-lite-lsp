(ns clojure-lite-lsp.kinds
  "The numbering of element kinds, flags and languages stored in the index.
  Shared by the writer and the readers; changing any number means bumping
  `clojure-lite-lsp.schema/version`."
  (:require
   [clojure.set :as set]))

(def ^:private kind-codes
  {:var-def 1
   :ns-def 2
   :keyword-def 3
   :protocol-impl 4
   :java-class-def 5
   :var-usage 10
   :ns-usage 11
   :ns-alias 12
   :keyword-usage 13
   :java-class-usage 14
   :symbol-usage 15
   :local 20
   :local-usage 21})

(def ^:private code-kinds (set/map-invert kind-codes))

(defn code
  "The stored number of `kind`; throws on an unknown kind."
  [kind]
  (or (kind-codes kind)
      (throw (ex-info (str "Unknown element kind " kind) {:kind kind}))))

(defn kind
  "The kind stored as number `n`."
  [n]
  (code-kinds n))

(def definition-kinds
  "Kinds stored as definitions."
  #{:var-def :ns-def :keyword-def :protocol-impl})

(def usage-kinds
  "Kinds stored as usages."
  #{:var-usage :ns-usage :ns-alias :keyword-usage :java-class-usage :symbol-usage})

(def searchable-kinds
  "Definitions whose names workspace symbol search finds."
  #{:var-def :ns-def :keyword-def})

(def ^:private flag-bits
  {:private 1 :macro 2 :deprecated 4 :defmethod 8 :fully-qualified 16 :unresolved 32
   ;; an ns-usage whose names are all referred (:refer :all, :use)
   :refer-all 64})

(def ^:private lang-bits {:clj 1 :cljs 2})

(defn- encode
  "The bits of `xs` in `table`; throws on one it doesn't have."
  [table what xs]
  (reduce (fn [bits x]
            (bit-or bits (or (table x) (throw (ex-info (str "Unknown " (name what) " " x) {what x})))))
          0 xs))

(defn- decode
  "The set of `table`'s keys whose bits are in `bits`."
  [table bits]
  (into #{} (keep (fn [[x b]] (when-not (zero? (bit-and bits b)) x))) table))

(defn flags->bits "`flags` as stored." [flags] (encode flag-bits :flag flags))
(defn bits->flags "Stored flag `bits` as flags." [bits] (decode flag-bits bits))
(defn langs->bits "`langs` as stored." [langs] (encode lang-bits :lang langs))
(defn bits->langs "Stored language `bits` as languages." [bits] (decode lang-bits bits))
