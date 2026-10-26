(ns harmoneer.exercise
  (:require [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.solver :as solver]))

;; Exercises are generated: a progression from a table of usual successions, a bass line under it,
;; then a voicing from the solver. The student gets the bass (with figures) or the soprano.
;; Chords are half notes, two to a 4/4 bar; the last chord is a whole note on a downbeat, so there
;; are 2 × bars − 1 chords and even columns are strong beats.

(def successions
  "Degree → weighted next degrees."
  {0 {3 3 4 3 5 2 1 2 2 1 6 1}
   1 {4 4 6 1}
   2 {5 2 3 2}
   3 {4 3 0 2 1 2 6 1}
   4 {0 4 5 2}
   5 {1 3 3 3 4 1}
   6 {0 1}})

(defn- pick [rnd pairs]
  (let [pairs (filter #(pos? (second %)) pairs)
        total (reduce + (map second pairs))
        r (* (rnd) total)]
    (when (seq pairs)
      (loop [[[x w] & more] pairs acc 0]
        (if (or (empty? more) (< r (+ acc w))) x (recur more (+ acc w)))))))

(defn- ok? [key level [d inv]] (h/allowed? key level {:d d :inv inv}))

(defn- next-chord [rnd key level [d inv] i]
  (let [pairs (for [[nd w] (successions d)
                    ninv [0 1]
                    :when (and (ok? key level [nd ninv]) (not= nd d))]
                [[nd ninv] (* w (if (zero? ninv) 3 2))])
        ;; a chord may repeat in another inversion from a strong beat to a weak one, never across the barline
        same (when (and (odd? i) (ok? key level [d (- 1 (min inv 1))]))
               [[[d (- 1 (min inv 1))] 2]])]
    (pick rnd (concat pairs same))))

(defn- six-fours
  "Passing and pedal 6/4 figures that can follow chord `c`: two chords each."
  [key level [d inv]]
  (filter #(every? (partial ok? key level) %)
          (case [d inv]
            [0 0] [[[4 2] [0 1]] [[3 2] [0 0]]]
            [0 1] [[[4 2] [0 0]]]
            [3 0] [[[0 2] [3 1]]]
            nil)))

(defn- cadence
  "The closing chords, ending on I."
  [rnd key level]
  (let [pre (pick rnd (for [c [[3 0] [3 1] [1 1] [1 0]] :when (ok? key level c)] [c (if (= (first c) 3) 2 3)]))]
    (if (>= level 4)
      (pick rnd [[[pre [0 2] [4 0] [0 0]] 5] [[pre [4 0] [0 0]] 3] [[[0 1] [3 0] [0 0]] 2]])
      [pre [4 0] [0 0]])))

(defn progression
  "`n` chords (odd), starting and ending on I in root position."
  [rnd key level n]
  (let [end (cadence rnd key level)
        body-n (- n (count end))]
    (loop [tries 0]
      (let [body (loop [cs [[0 0]]]
                   (cond (= (count cs) body-n) cs
                         (> (count cs) body-n) nil
                         :else
                         (let [i (count cs)
                               c (peek cs)
                               figs (six-fours key level c)]
                           (if (and (seq figs) (< (rnd) 0.2) (<= (+ i 2) body-n))
                             (recur (into cs (nth figs (Math/floor (* (rnd) (count figs))))))
                             (if-let [nc (next-chord rnd key level c i)]
                               (recur (conj cs nc))
                               nil)))))
            joint (when body [(peek body) (first end)])
            fits? (and body
                       (pos? (get-in successions [(first (first joint)) (first (second joint))] 0))
                       (not (and (= 2 (second (first end))) (odd? body-n)))
                       (not= 2 (second (peek body))))]
        (cond fits? (mapv (fn [[d inv]] {:d d :inv inv}) (concat body end))
              (< tries 200) (recur (inc tries))
              :else nil)))))

(defn- choose [rnd xs] (nth xs (Math/floor (* (rnd) (count xs)))))

(defn bass-line
  "Bass notes for `chords`, mostly near the previous one; around a 6/4 always the nearest, so
   passing and pedal figures step or hold."
  [rnd key chords]
  (let [opts (fn [c lo hi]
               (for [s (range 14 32)
                     :let [{:keys [l a]} ((h/tones key c) (:inv c)) n {:s s :a a}]
                     :when (and (= l (mod s 7)) (<= lo (t/midi n) hi))]
                 n))
        nearest (fn [prev c] (apply min-key #(Math/abs (- (t/midi %) (t/midi prev))) (opts c 40 58)))]
    (reduce
     (fn [line c]
       (let [prev (peek line)
             p (get chords (dec (count line)))
             good (for [n (opts c 40 58)
                        :let [iv (t/interval prev n)]
                        :when (and (<= (:g iv) 7) (not (#{:A :AA :d :dd} (:q iv))) (not= 6 (:g7 iv)))]
                    [n (/ 1 (+ 1 (* (:g iv) (:g iv))))])]
         (conj line (if (or (= 2 (:inv c)) (= 2 (:inv p)))
                      (nearest prev c)
                      (or (pick rnd good) (nearest prev c))))))
     [(choose rnd (opts (first chords) 41 52))]
     (rest chords))))

(defn given-voice [type] (if (= type :bass) :b :s))

(defn generate
  "A new exercise, or nil if nothing clean turned up. `bars` is 4, 6 or 8."
  [rnd {:keys [key level type bars]}]
  (let [n (dec (* 2 bars))
        given (given-voice type)]
    (loop [tries 0]
      (let [chords (progression rnd key level n)
            bass (when chords (bass-line rnd key chords))
            sol (when bass (solver/solve key chords (mapv (fn [b] {:b b}) bass) rnd))
            ex {:key key :level level :type type :chords chords :given given}
            vs (when sol (rules/analyze (assoc ex :cols (:cols sol) :type :bass) (rules/preset-rules :strict)))]
        (cond (and sol (not-any? #(= :error (:level %)) vs) (<= (count vs) 1))
              (assoc ex :solution (:cols sol) :cols (mapv #(select-keys % [given]) (:cols sol)))
              (< tries 40) (recur (inc tries))
              :else nil)))))
