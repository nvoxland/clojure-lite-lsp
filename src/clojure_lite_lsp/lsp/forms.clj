(ns clojure-lite-lsp.lsp.forms
  "Just enough reading of Clojure text for signature help: which call the
  cursor is in, and which argument, from the text before it (the buffer as
  typed, not the index). Strings, comments, character literals and reader
  prefixes are skipped over; nothing is parsed into data."
  (:require
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private openers #{\( \[ \{})
(def ^:private closers #{\) \] \}})
(def ^:private prefixes #{\' \` \~ \@})

(defn- whitespace? [c] (or (Character/isWhitespace (char c)) (= \, c)))

(defn- update-top
  "`stack` with its innermost frame updated by (f frame & args); unchanged
  when empty."
  [stack f & args]
  (if (seq stack)
    (conj (pop stack) (apply f (peek stack) args))
    stack))

(defn- start-form
  "A form starts in the innermost frame at `i`: counted, unless it's the
  metadata a ^ announced, or the form a prefix already counted."
  [stack i token?]
  (update-top stack
              (fn [{:keys [skip prefixed forms open] :as frame}]
                (cond
                  prefixed (dissoc frame :prefixed)
                  (pos? (or skip 0)) (update frame :skip dec)
                  :else (cond-> (update frame :forms inc)
                          ;; a list's first form is its head, when a symbol
                          (and (= \( open) (zero? forms)) (assoc :head (when token? [i nil])))))))

(defn- end-token
  "The token that ended at `i`: when it was a list's head, where it ends."
  [stack i]
  (update-top stack (fn [{:keys [head] :as frame}]
                      (cond-> frame
                        (and head (nil? (second head))) (assoc-in [:head 1] i)))))

(defn- skip-next
  "The innermost frame's next form isn't one of its forms (^metadata, #_)."
  [stack]
  (update-top stack update :skip (fnil inc 0)))

(defn- prefix
  "The form just counted continues in what follows (a reader prefix)."
  [stack]
  (update-top stack assoc :prefixed true))

(defn- token-end
  "Where the token starting at `i` ends: a tag (#inst), a namespaced map's
  :ns, ##Inf."
  [^String text i n]
  (loop [j i]
    (if (and (< j n)
             (let [c (.charAt text j)]
               (not (or (whitespace? c) (openers c) (closers c) (= \" c)))))
      (recur (inc j))
      j)))

(defn- string-end
  "Where the string whose opening quote is at `i` ends (just after its
  closing quote), or nil when it isn't closed before `n`."
  [^String text i n]
  (loop [j (inc i)]
    (cond (>= j n) nil
          (= \\ (.charAt text j)) (recur (+ j 2))
          (= \" (.charAt text j)) (inc j)
          :else (recur (inc j)))))

(defn- scan
  "Read `text` up to `n`, returning {:stack :token}: the open lists, vectors
  and maps (each with its form count and a list's head), and where the
  token the scan ends in started. With `close-at`, stops instead when the
  frame opened at that index closes, returning {:closed frame}."
  ([text n] (scan text n nil))
  ([^String text n close-at]
   (loop [i 0 stack [] token nil]
     (if (< i n)
       (let [c (.charAt text i)]
         (cond
          ;; a character literal: \( \space \newline ...
           (and (= \\ c) (nil? token))
           (recur (+ i 2) (start-form stack i true) i)

           token
           (if (or (whitespace? c) (openers c) (closers c) (= \; c) (= \" c))
             (recur i (end-token stack i) nil)
             (recur (inc i) stack token))

           (whitespace? c) (recur (inc i) stack nil)

           (= \; c) (let [nl (.indexOf text "\n" (int i))]
                      (recur (if (neg? nl) n nl) stack nil))

           (= \" c) (let [end (string-end text i n)]
                     ;; unclosed at the end: in that form
                      (recur (or end n) (start-form stack i false) (when-not end i)))

           (openers c) (recur (inc i) (conj (start-form stack i false) {:open c :forms 0 :at i}) nil)

           (closers c) (if (and close-at (= close-at (:at (peek stack))))
                         {:closed (peek stack)}
                         (recur (inc i) (if (seq stack) (pop stack) stack) nil))

           (= \^ c) (recur (inc i) (skip-next stack) nil)

           (= \# c) (let [d (when (< (inc i) n) (.charAt text (inc i)))]
                      (cond
                       ;; #_ discards the next form
                        (= \_ d) (recur (+ i 2) (skip-next stack) nil)
                       ;; ##Inf ##NaN: a whole form, prefixing nothing
                        (= \# d) (recur (token-end text i n) (start-form stack i false) nil)
                       ;; #? #?@ #' #( #{ #"..." #:ns{...} #tag form: one
                       ;; form, whatever follows the dispatch
                        :else (let [stack (prefix (start-form stack i false))
                                    after (cond
                                            (#{\( \{ \"} d) (inc i)
                                            (= \? d) (if (and (< (+ i 2) n) (= \@ (.charAt text (+ i 2)))) (+ i 3) (+ i 2))
                                            (= \' d) (+ i 2)
                                            :else (token-end text (inc i) n))]
                                (recur after stack nil))))

           (prefixes c) (recur (inc i) (prefix (start-form stack i false)) nil)

           :else (recur (inc i) (start-form stack i true) i)))
       {:stack stack :token token}))))

(defn call-at
  "The call the cursor at `offset` in `text` is in: {:head [start end] (of
  the called symbol) :arg n (0-based argument index, 0 on the name itself)
  :count n (the call's arguments, before and after the cursor)}, or nil
  when it isn't in a call whose head is a symbol."
  [^String text offset]
  (let [n (min offset (count text))
        {:keys [stack token]} (scan text n)
        call (last (keep-indexed (fn [i {:keys [open]}] (when (= \( open) i)) stack))]
    (when call
      (let [{:keys [forms head prefixed at]} (nth stack call)
            [start end] head
            ;; in the middle of a form: that form; else the next one
            in-form? (or (< call (dec (count stack))) token prefixed)
            arg (if in-form? (- forms 2) (dec forms))
            ;; the whole call: on to where it closes (or the text ends)
            {:keys [closed] after :stack} (scan text (count text) at)
            forms-in-call (:forms (or closed (some #(when (= at (:at %)) %) after)))]
        (when start
          {:head [start (or end (token-end text start (count text)))]
           :arg (max 0 arg)
           :count (max 0 (dec (or forms-in-call forms)))})))))

(defn- top-level-tokens
  "`s` split on whitespace outside brackets."
  [^String s]
  (loop [i 0 depth 0 start nil tokens []]
    (if (< i (count s))
      (let [c (.charAt s i)]
        (cond
          (and (zero? depth) (whitespace? c))
          (recur (inc i) depth nil (cond-> tokens start (conj (subs s start i))))
          (openers c) (recur (inc i) (inc depth) (or start i) tokens)
          (closers c) (recur (inc i) (dec depth) start tokens)
          :else (recur (inc i) depth (or start i) tokens)))
      (cond-> tokens start (conj (subs s start))))))

(defn arglist-params
  "The parameters of an arglist string like \"[a b & more]\": {:params
  [\"a\" \"b\" \"more\"] :variadic 2} (the index of the rest parameter, or
  nil)."
  [arglist]
  (let [s (str/trim arglist)
        inner (if (and (str/starts-with? s "[") (str/ends-with? s "]")) (subs s 1 (dec (count s))) s)
        ;; type hints (^String s, ^{:tag X} s) aren't parameters
        tokens (remove #(str/starts-with? % "^") (top-level-tokens inner))]
    {:params (into [] (remove #{"&"}) tokens)
     :variadic (first (keep-indexed #(when (= "&" %2) %1) tokens))}))
