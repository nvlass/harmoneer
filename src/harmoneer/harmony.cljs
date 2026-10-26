(ns harmoneer.harmony
  (:require [clojure.set :as set]
            [harmoneer.theory :as t]))

;; A chord is {:d degree 0–6 :inv 0|1|2}. Chord tones have roles 0 (root), 1 (third), 2 (fifth).
;; A column is {:s :a :t :b} → note or nil. Minor keys use harmonic minor, so V is major,
;; VII diminished and III augmented.

(def voices [:s :a :t :b])
(def upper [:s :a :t])
(def pairs [[:s :a] [:s :t] [:s :b] [:a :t] [:a :b] [:t :b]])

(def ranges
  "MIDI ranges per voice."
  {:s [60 79] :a [55 74] :t [48 67] :b [40 60]})

(defn root [key d]
  (let [l (mod (+ (get-in key [:tonic :l]) d) 7)]
    {:l l :a (t/scale-alt key l)}))

(defn tones
  "The chord's pitch classes by role, as {:l :a}."
  [key {:keys [d]}]
  (vec (for [k [0 2 4]
             :let [l (mod (+ (get-in key [:tonic :l]) d k) 7)]]
         {:l l :a (t/scale-alt key l)})))

(defn role
  "The role of `note` in chord degree `d`, or nil."
  [key d note]
  (first (keep-indexed (fn [r {:keys [l a]}] (when (and (= l (t/letter note)) (= a (:a note))) r))
                       (tones key {:d d}))))

(defn leading-tone? [key n]
  (and (= (t/letter n) (mod (+ (get-in key [:tonic :l]) 6) 7))
       (= (:a n) (t/scale-alt key (t/letter n)))))

(defn quality
  "Triad quality from its third and fifth."
  [key chord]
  (let [[r th fi] (map #(t/midi {:s (:l %) :a (:a %)}) (tones key chord))
        i3 (mod (- th r) 12) i5 (mod (- fi r) 12)]
    (case [i3 i5] [4 7] :major [3 7] :minor [3 6] :dim [4 8] :aug :other)))

(defn present [notes] (keep identity (map notes voices)))

(defn identify
  "The chord the notes of a column spell in `key`, with the root present, or nil."
  [key col]
  (let [ns (present col)]
    (when (seq ns)
      (first (for [d (range 7)
                   :let [rs (map #(role key d %) ns)]
                   :when (and (every? some? rs) (some zero? rs))]
               {:d d :inv (if-let [b (:b col)] (role key d b) 0)})))))

(defn roles
  "Each present voice's role in `chord`, nil for notes outside it."
  [key chord col]
  (into {} (for [v voices :let [n (col v)] :when n] [v (role key (:d chord) n)])))

;; ---------------------------------------------------------------- figures and numerals

(defn figures
  "Figured-bass rows for `chord` over `bass`, top first: numbers with an accidental where the
   note differs from the key signature. A root-position chord shows only what needs an accidental;
   `after-64?` writes out 5/3 after a cadential 6/4."
  [key chord bass after-64?]
  (let [ts (tones key chord)
        bl (t/letter bass)
        above (for [{:keys [l a]} ts :when (not= l bl)]
                (let [num (inc (mod (- l bl) 7))
                      sig (t/sig-alt key l)]
                  {:num num :acc (when (not= a sig) a)}))
        by-num (into {} (map (juxt :num identity) above))
        shown (case (:inv chord)
                0 (if after-64? [5 3] (filter #(:acc (by-num %)) [5 3]))
                1 (cond-> [6] (:acc (by-num 3)) (conj 3))
                2 [6 4])]
    ;; a raised or lowered third is written as the accidental alone
    (vec (for [num shown :let [acc (:acc (by-num num))]]
           {:num (when-not (and (= num 3) acc) num) :acc acc}))))

(def numerals ["I" "II" "III" "IV" "V" "VI" "VII"])

(defn numeral [key {:keys [d] :as chord}]
  (str (numerals d) (case (quality key chord) :dim "°" :aug "+" "")))

(def inversion-figure {0 nil 1 "6" 2 "6/4"})

;; ---------------------------------------------------------------- levels

(def levels
  "Chords each level adds, following Kanas's first year: 1 primary degrees, 2 their inversions,
   3 VI and II, 4 cadences (cadential 6/4, plagal, deceptive), 5 III and VII."
  {1 #{[0 0] [3 0] [4 0]}
   2 #{[0 1] [3 1] [4 1] [0 2] [3 2] [4 2]}
   3 #{[5 0] [5 1] [1 0] [1 1]}
   4 #{}
   5 #{[2 0] [6 1]}})

(def max-level 5)

(defn vocab [level] (apply set/union (for [l (range 1 (inc level))] (levels l))))

(defn allowed? [key level {:keys [d inv] :as chord}]
  (and (contains? (vocab level) [d inv])
       (not (and (= (:mode key) :minor) (= [d inv] [1 0])))))

;; ---------------------------------------------------------------- doubling

(defn doubled
  "Roles that appear more than once among the present notes."
  [rs]
  (set (for [[r k] (frequencies (vals rs)) :when (and r (> k 1))] r)))

(defn good-doublings
  "Roles that may be doubled in `chord`; `prev` is the chord before (for VI after V)."
  [key {:keys [d inv]} prev]
  (case inv
    0 (cond (= d 6) #{1}
            (and (= d 1) (= :dim (quality key {:d 1}))) #{1}
            (and (= d 5) (= 4 (:d prev))) #{0 1}
            :else #{0})
    1 (case d
        (0 3 4) #{0 2}
        1 #{0 1}
        6 #{1 2}
        #{0 1 2})
    2 #{2}))
