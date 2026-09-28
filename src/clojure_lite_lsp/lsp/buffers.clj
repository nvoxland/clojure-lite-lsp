(ns clojure-lite-lsp.lsp.buffers
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

(defn offset
  "The index in `text` of an LSP position. Characters are UTF-16 units,
  which is how Java strings count."
  [^String text {:keys [line character]}]
  (loop [i 0 l 0]
    (if (= l line)
      ;; past the line's end means its end
      (let [nl (.indexOf text "\n" (int i))
            end (if (neg? nl) (count text) nl)]
        (min end (+ i character)))
      (let [nl (.indexOf text "\n" (int i))]
        (if (neg? nl) (count text) (recur (inc nl) (inc l)))))))

(defn apply-change
  "`text` after one LSP content change: a range replaced, or (no range)
  the whole text."
  [text {:keys [range] new-text :text}]
  (if range
    (str (subs text 0 (offset text (:start range))) new-text (subs text (offset text (:end range))))
    new-text))

(defn change!
  "Apply LSP content changes, in order, to the document at `path`."
  [store path changes]
  (swap! store update path
         (fn [{:keys [base current] :as e}]
           (when e
             (let [text (reduce apply-change current changes)]
               (merge (entry base text) (select-keys e [:saved :saved-hash])))))))

(defn- sha256 ^bytes [^String s]
  (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(defn- await-text
  "The file now holds `text`: it becomes the base once the index has it
  (`indexed!`); until then, the base stays what the index has."
  [{:keys [base] :as e} text]
  (when e
    (if (= base text)
      (dissoc e :saved :saved-hash)
      (assoc e :saved text :saved-hash (sha256 text)))))

(defn saved!
  "The document was saved: the buffer is what the file holds."
  [store path]
  (swap! store update path #(await-text % (:current %))))

(defn changed-on-disk!
  "The file changed outside the editor (a checkout, a formatter) and holds
  `text`."
  [store path text]
  (swap! store update path #(await-text % text)))

(defn awaiting
  "{path content-hash} of the documents whose file holds text the index
  may not have yet."
  [store]
  (into {} (keep (fn [[path {:keys [saved-hash]}]] (when saved-hash [path saved-hash]))) @store))

(defn indexed!
  "The index now has `path` as it was saved: that is the base."
  [store path]
  (swap! store update path
         (fn [{:keys [saved current] :as e}]
           (if (and e saved) (entry saved current) e))))

(defn close! [store path] (swap! store dissoc path))

(defn text
  "The text of `path` as the editor has it: its open buffer, else the file."
  [store path]
  (or (:current (@store path))
      (let [f (io/file path)] (when (.isFile f) (slurp f)))))

(defn position
  "The LSP position of index `i` in `text`."
  [^String text i]
  (let [before (subs text 0 (min i (count text)))
        nl (.lastIndexOf before "\n")]
    {:line (count (filter #(= \newline %) before)) :character (- (count before) (inc nl))}))

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
