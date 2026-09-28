(ns clojure-lite-lsp.lsp.buffers
  "Open documents, and mapping positions between an edited buffer and the
  version the index has (DESIGN.md §6.8).

  The index only knows saved files. While a buffer has unsaved edits, a
  line diff between the saved text (the base) and the buffer maps
  positions both ways; positions on changed lines have no counterpart and
  map to nil."
  (:require
   [clojure-lite-lsp.digest :as digest]
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
  (let [n (count a)
        m (count b)
        w (inc m)
        t (int-array (* (inc n) w))
        at (fn [i j] (aget t (+ (* i w) j)))]
    (dotimes [k n]
      (let [i (- n 1 k)]
        (dotimes [l m]
          (let [j (- m 1 l)]
            (aset t (+ (* i w) j)
                  (int (if (= (a i) (b j))
                         (inc (at (inc i) (inc j)))
                         (max (at (inc i) j) (at i (inc j))))))))))
    (loop [i 0 j 0 pairs []]
      (cond
        (or (= i n) (= j m)) pairs
        (= (a i) (b j)) (recur (inc i) (inc j) (conj pairs [i j]))
        (>= (at (inc i) j) (at i (inc j))) (recur (inc i) j pairs)
        :else (recur i (inc j) pairs)))))

(defn line-map
  "How the lines of `base` and `current` correspond (0-based lines)."
  [base current]
  (let [b (vec (str/split-lines base))
        c (vec (str/split-lines current))
        nb (count b)
        nc (count c)
        ;; the unchanged lines at the start and at the end
        p (loop [p 0] (if (and (< p (min nb nc)) (= (b p) (c p))) (recur (inc p)) p))
        s (loop [s 0] (if (and (< s (- (min nb nc) p)) (= (b (- nb 1 s)) (c (- nc 1 s)))) (recur (inc s)) s))
        mb (subvec b p (- nb s))
        mc (subvec c p (- nc s))
        middle (when (<= (* (count mb) (count mc)) lcs-cell-limit) (lcs-pairs mb mc))
        pairs (concat (map (fn [i] [i i]) (range p))
                      (map (fn [[i j]] [(+ p i) (+ p j)]) middle)
                      (map (fn [k] [(- nb 1 k) (- nc 1 k)]) (range s)))]
    {:to-base (into {} (map (fn [[i j]] [j i])) pairs)
     :to-current (into {} pairs)}))

(defn current->base [m line] (get (:to-base m) line))
(defn base->current [m line] (get (:to-current m) line))

;;;; open documents

(defn store
  "A store of open documents, for the functions below."
  []
  (atom {}))

(defn- entry [base current]
  {:base base :current current :map (delay (line-map base current))})

(defn- update-open!
  "Apply (f entry & args) to the entry of `path`, when it's open."
  [buffers path f & args]
  (swap! buffers #(if (contains? % path) (apply update % path f args) %)))

(defn open!
  "Track a document opened with `text`. Its base is the file as saved (what
  the index has), when there is one."
  [buffers path text]
  (let [f (io/file path)
        base (if (.isFile f) (slurp f) text)]
    (swap! buffers assoc path (entry base text))))

(defn offset
  "The index in `text` of an LSP position. Characters are UTF-16 units,
  which is how Java strings count."
  [^String text {:keys [line character]}]
  (loop [i 0 l 0]
    (let [nl (.indexOf text "\n" (int i))
          end (if (neg? nl) (count text) nl)]
      (cond
        ;; past the line's end means its end
        (= l line) (min end (+ i character))
        (neg? nl) (count text)
        :else (recur (inc nl) (inc l))))))

(defn apply-change
  "`text` after one LSP content change: a range replaced, or (no range)
  the whole text."
  [text {:keys [range] new-text :text}]
  (if range
    (str (subs text 0 (offset text (:start range))) new-text (subs text (offset text (:end range))))
    new-text))

(defn change!
  "Apply LSP content changes, in order, to the document at `path`."
  [buffers path changes]
  (update-open! buffers path
                (fn [{:keys [base current] :as e}]
                  (merge (entry base (reduce apply-change current changes))
                         (select-keys e [:saved :saved-hash])))))

(defn- await-text
  "The file now holds `text`: it becomes the base once the index has it
  (`indexed!`); until then, the base stays what the index has."
  [{:keys [base] :as e} text]
  (if (= base text)
    (dissoc e :saved :saved-hash)
    (assoc e :saved text :saved-hash (digest/sha256 text))))

(defn saved!
  "The document was saved: the buffer is what the file holds."
  [buffers path]
  (update-open! buffers path #(await-text % (:current %))))

(defn changed-on-disk!
  "The file changed outside the editor (a checkout, a formatter) and holds
  `text`."
  [buffers path text]
  (update-open! buffers path await-text text))

(defn awaiting
  "{path content-hash} of the documents whose file holds text the index
  may not have yet."
  [buffers]
  (into {} (keep (fn [[path {:keys [saved-hash]}]] (when saved-hash [path saved-hash]))) @buffers))

(defn indexed!
  "The index now has `path` as it was saved: that is the base."
  [buffers path]
  (update-open! buffers path (fn [{:keys [saved current] :as e}]
                               (if saved (entry saved current) e))))

(defn close! [buffers path] (swap! buffers dissoc path))

(defn text
  "The text of `path` as the editor has it: its open buffer, else the file."
  [buffers path]
  (or (:current (@buffers path))
      (let [f (io/file path)] (when (.isFile f) (slurp f)))))

(defn position
  "The LSP position of index `i` in `text`."
  [^String text i]
  (let [before (subs text 0 (min i (count text)))
        nl (.lastIndexOf before "\n")]
    {:line (count (filter #(= \newline %) before)) :character (- (count before) (inc nl))}))

(defn- map-rows
  "`pos` ([row col ...], 1-based) with each row mapped through `f` (a line
  map and a 0-based line to a line), for the buffer at `path`; nil when a
  row has no counterpart."
  [buffers path pos f]
  (let [{m :map :keys [base current]} (@buffers path)]
    (if (or (nil? m) (= base current))
      pos
      (reduce (fn [pos k]
                (if-let [line (f @m (dec (pos k)))]
                  (assoc pos k (inc line))
                  (reduced nil)))
              (vec pos)
              (range 0 (count pos) 2)))))

(defn ->indexed
  "A position ([row col] or [row col end-row end-col], 1-based) in the
  buffer at `path` as a position in the indexed version, or nil."
  [buffers path pos]
  (map-rows buffers path pos current->base))

(defn ->buffer
  "A position in the indexed version of `path` as one in its buffer, or nil."
  [buffers path pos]
  (map-rows buffers path pos base->current))
