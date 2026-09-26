(ns csl.lsp.buffers
  "Open documents, and mapping positions between an edited buffer and the
  version the index has (DESIGN.md §6.8).

  The index only knows saved files. While a buffer has unsaved edits, a
  line diff between the saved text (the base) and the buffer maps
  positions both ways; positions on changed lines have no counterpart and
  map to nil."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private lcs-cell-limit
  "Above this many cells, a changed middle isn't matched line by line
  (its lines map to nothing; the unchanged ends still map)."
  1000000)

(defn- lcs-pairs
  "Pairs [i j] of equal lines matched by a longest common subsequence of
  vectors `a` and `b`."
  [a b]
  (let [n (count a) m (count b)
        w (inc m)
        t (int-array (* (inc n) w))]
    (doseq [i (range (dec n) -1 -1), j (range (dec m) -1 -1)]
      (aset t (+ (* i w) j)
            (if (= (a i) (b j))
              (inc (aget t (+ (* (inc i) w) (inc j))))
              (max (aget t (+ (* (inc i) w) j)) (aget t (+ (* i w) (inc j)))))))
    (loop [i 0 j 0 acc []]
      (cond
        (or (= i n) (= j m)) acc
        (= (a i) (b j)) (recur (inc i) (inc j) (conj acc [i j]))
        (>= (aget t (+ (* (inc i) w) j)) (aget t (+ (* i w) (inc j)))) (recur (inc i) j acc)
        :else (recur i (inc j) acc)))))

(defn line-map
  "How the lines of `base` and `current` correspond (0-based lines)."
  [base current]
  (let [b (vec (str/split-lines base))
        c (vec (str/split-lines current))
        nb (count b) nc (count c)
        p (loop [p 0] (if (and (< p (min nb nc)) (= (b p) (c p))) (recur (inc p)) p))
        s (loop [s 0] (if (and (< s (- (min nb nc) p)) (= (b (- nb 1 s)) (c (- nc 1 s)))) (recur (inc s)) s))
        mb (subvec b p (- nb s))
        mc (subvec c p (- nc s))
        middle (when (<= (* (count mb) (count mc)) lcs-cell-limit) (lcs-pairs mb mc))
        pairs (concat (map (fn [i] [i i]) (range p))
                      (map (fn [[i j]] [(+ p i) (+ p j)]) middle)
                      (map (fn [k] [(- nb 1 k) (- nc 1 k)]) (range s)))]
    {:to-base (reduce (fn [acc [i j]] (assoc acc j i)) {} pairs)
     :to-current (reduce (fn [acc [i j]] (assoc acc i j)) {} pairs)}))

(defn current->base [m line] (get (:to-base m) line))
(defn base->current [m line] (get (:to-current m) line))

;;;; open documents

(defn store [] (atom {}))

(defn- entry [base current]
  {:base base :current current :map (delay (line-map base current))})

(defn open!
  "Track a document opened with `text`. Its base is the file as saved (what
  the index has), when there is one."
  [store path text]
  (let [f (io/file path)
        base (if (.isFile f) (slurp f) text)]
    (swap! store assoc path (entry base text))))

(defn change! [store path text]
  (swap! store update path #(entry (or (:base %) text) text)))

(defn saved!
  "The document was saved: the buffer is now the base."
  [store path]
  (swap! store update path #(when % (entry (:current %) (:current %)))))

(defn close! [store path] (swap! store dissoc path))

(defn- shift [store path pos f]
  (if-let [{m :map :keys [base current]} (@store path)]
    (if (= base current)
      pos
      (let [rows (map #(f @m (dec %)) (take-nth 2 pos))]
        (when (every? some? rows)
          (vec (interleave (map inc rows) (take-nth 2 (rest pos)))))))
    pos))

(defn ->indexed
  "A position ([row col] or [row col end-row end-col], 1-based) in the
  buffer at `path` as a position in the indexed version, or nil."
  [store path pos]
  (shift store path pos current->base))

(defn ->buffer
  "A position in the indexed version of `path` as one in its buffer, or nil."
  [store path pos]
  (shift store path pos base->current))
