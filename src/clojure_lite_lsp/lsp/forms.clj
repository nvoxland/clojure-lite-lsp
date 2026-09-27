(ns clojure-lite-lsp.lsp.forms
  "Just enough reading of Clojure text for signature help: which call the
  cursor is in, and which argument, from the text before it (the buffer as
  typed, not the index). Strings, comments, character literals and reader
  prefixes are skipped over; nothing is parsed into data.")

(set! *warn-on-reflection* true)

(def ^:private openers #{\( \[ \{})
(def ^:private closers #{\) \] \}})
(def ^:private prefixes #{\' \` \~ \@ \# \^})

(defn- whitespace? [c] (or (Character/isWhitespace (char c)) (= \, c)))

(defn- start-form
  "A form starts in the innermost frame at `i`: counted, unless it's the
  metadata a ^ announced, or the form a prefix already counted."
  [stack i token?]
  (if-let [{:keys [skip prefixed forms open]} (peek stack)]
    (cond
      prefixed (update stack (dec (count stack)) dissoc :prefixed)
      (pos? (or skip 0)) (update-in stack [(dec (count stack)) :skip] dec)
      :else (let [frame (peek stack)
                  frame (cond-> (assoc frame :forms (inc forms))
                          ;; a list's first form is its head, when a symbol
                          (and (= \( open) (zero? forms)) (assoc :head (when token? [i nil])))]
              (conj (pop stack) frame)))
    stack))

(defn- end-token
  "The token that ended at `i`: when it was a list's head, where it ends."
  [stack i]
  (if-let [{:keys [head]} (peek stack)]
    (if (and head (nil? (second head)))
      (assoc-in stack [(dec (count stack)) :head 1] i)
      stack)
    stack))

(defn call-at
  "The call the cursor at `offset` in `text` is in: {:head [start end] (of
  the called symbol) :arg n (0-based argument index)}, or nil when it isn't
  in a call whose head is a symbol."
  [^String text offset]
  (let [n (min offset (count text))]
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

            (= \" c) (let [end (loop [j (inc i)]
                                 (cond (>= j n) n
                                       (= \\ (.charAt text j)) (recur (+ j 2))
                                       (= \" (.charAt text j)) (inc j)
                                       :else (recur (inc j))))]
                       (recur end (start-form stack i false) nil))

            (openers c) (recur (inc i) (conj (start-form stack i false) {:open c :forms 0}) nil)

            (closers c) (recur (inc i) (if (seq stack) (pop stack) stack) nil)

            (= \^ c) (let [stack (if (seq stack) (update-in stack [(dec (count stack)) :skip] (fnil inc 0)) stack)]
                       (recur (inc i) stack nil))

            (prefixes c) (let [stack (start-form stack i false)
                               stack (if (seq stack) (assoc-in stack [(dec (count stack)) :prefixed] true) stack)]
                           (recur (inc i) stack nil))

            :else (recur (inc i) (start-form stack i true) i)))
        ;; at the cursor
        (let [innermost (dec (count stack))
              call (first (filter #(= \( (:open (nth stack %))) (range innermost -1 -1)))]
          (when call
            (let [{:keys [forms head prefixed]} (nth stack call)
                  [start end] head
                  ;; in the middle of a form: that form; else the next one
                  in-form? (or (< call innermost) token prefixed)
                  arg (if in-form? (- forms 2) (dec forms))]
              (when (and start (>= arg 0))
                {:head [start (or end n)] :arg arg}))))))))

(defn arglist-params
  "The parameters of an arglist string like \"[a b & more]\": {:params
  [\"a\" \"b\" \"more\"] :variadic 2} (the index of the rest parameter, or
  nil)."
  [^String arglist]
  (let [inner (let [s (.trim arglist)]
                (if (and (.startsWith s "[") (.endsWith s "]")) (subs s 1 (dec (count s))) s))
        tokens (loop [i 0 depth 0 start nil out []]
                 (if (< i (count inner))
                   (let [c (.charAt ^String inner i)]
                     (cond
                       (and (zero? depth) (whitespace? c))
                       (recur (inc i) depth nil (cond-> out start (conj (subs inner start i))))
                       (openers c) (recur (inc i) (inc depth) (or start i) out)
                       (closers c) (recur (inc i) (dec depth) start out)
                       :else (recur (inc i) depth (or start i) out)))
                   (cond-> out start (conj (subs inner start)))))
        amp (.indexOf ^java.util.List tokens "&")]
    {:params (vec (remove #{"&"} tokens))
     :variadic (when-not (neg? amp) amp)}))
