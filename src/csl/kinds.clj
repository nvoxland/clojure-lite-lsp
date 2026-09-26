(ns csl.kinds
  "The numbering of element kinds, flags and languages stored in the index.
  Shared by the writer and the readers; changing any number means bumping
  `csl.schema/version`.")

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

(def ^:private code-kinds (into {} (map (fn [[k v]] [v k])) kind-codes))

(defn code
  "The stored number of `kind`; throws on an unknown kind."
  [kind]
  (or (kind-codes kind)
      (throw (ex-info (str "Unknown element kind " kind) {:kind kind}))))

(defn kind [code] (code-kinds code))

(def definition-kinds #{:var-def :ns-def :keyword-def :protocol-impl})

(def usage-kinds #{:var-usage :ns-usage :ns-alias :keyword-usage :java-class-usage :symbol-usage})

(def searchable-kinds
  "Definitions whose names workspace symbol search finds."
  #{:var-def :ns-def :keyword-def})

(def ^:private flag-bits
  {:private 1 :macro 2 :deprecated 4 :defmethod 8 :fully-qualified 16 :unresolved 32})

(defn flags->bits [flags]
  (reduce (fn [acc f] (bit-or acc (or (flag-bits f)
                                      (throw (ex-info (str "Unknown flag " f) {:flag f})))))
          0 flags))

(defn bits->flags [bits]
  (into #{} (keep (fn [[f b]] (when (pos? (bit-and bits b)) f))) flag-bits))

(def ^:private lang-bits {:clj 1 :cljs 2})

(defn langs->bits [langs] (reduce + (map lang-bits langs)))

(defn bits->langs [bits]
  (into #{} (keep (fn [[l b]] (when (pos? (bit-and bits b)) l))) lang-bits))
