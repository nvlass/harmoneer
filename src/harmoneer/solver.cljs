(ns harmoneer.solver
  (:require [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]))

;; Four-part voicings for a fixed chord progression, by dynamic programming over columns:
;; a column's cost is its own rule penalties, a step's cost the penalties between two columns plus
;; how far the upper voices move. The rules are the same ones that check the student.

(def weights {:error 1000 :warning 25})

(def strict (rules/preset-rules :strict))

(defn- rules-of [check]
  (filter #(and (contains? strict (:id %)) (get-method check (:id %))
                (not= (get-method check (:id %)) (get-method check :default))
                (not (:types %)))
          rules/rules))

(def col-rules (rules-of rules/check-col))
(def pair-rules (rules-of rules/check-pair))

(defn- notes-in
  "Chord tones of `chord` within `[lo hi]`, optionally only those with role `r`."
  [key chord [lo hi] r]
  (let [ts (h/tones key chord)]
    (for [s (range (- (Math/floor (/ (* 7 lo) 12)) 2) (+ (Math/ceil (/ (* 7 hi) 12)) 2))
          :let [k (first (keep-indexed #(when (= (:l %2) (mod s 7)) %1) ts))]
          :when (and k (or (nil? r) (= r k)))
          :let [n {:s s :a (:a (ts k))} mm (t/midi n)]
          :when (<= lo mm hi)]
      n)))

(defn- cost-of [ctx check-fn rs & args]
  (reduce + (for [r rs v (apply check-fn (:id r) ctx args)] (weights (:level r)))))

(defn candidates
  "Voicings of column `i`, with the voices in `fixed` (voice → note) kept as they are."
  [ctx chord i fixed]
  (let [{:keys [key]} ctx
        opts (fn [v r] (if-let [n (fixed v)] [n] (notes-in key chord (h/ranges v) r)))]
    (for [b (opts :b (:inv chord))
          tn (opts :t nil) :when (<= (t/midi b) (t/midi tn) (+ (t/midi b) 19))
          an (opts :a nil) :when (<= (t/midi tn) (t/midi an) (+ (t/midi tn) 12))
          sn (opts :s nil) :when (<= (t/midi an) (t/midi sn) (+ (t/midi an) 12))
          :let [col {:s sn :a an :t tn :b b}
                x (rules/info key :bass (:chords ctx) i col)
                c (cost-of ctx rules/check-col col-rules x)]
          :when (< c 1000)]
      {:x x :cost c})))

(defn- smooth [a b]
  (+ (reduce + (for [v h/upper] (* 0.4 (Math/abs (- (t/midi (b v)) (t/midi (a v)))))))
     (let [g (:g (t/interval (a :s) (b :s)))]
       (cond (zero? g) 3 (= 1 g) 0 (= 2 g) 1.5 :else (* 4 (dec g))))
     (if (> (t/midi (b :s)) 76) 3 0)))

(defn solve
  "The cheapest voicing of `chords` in `key`, as {:cols :cost}, or nil. `fixed` is a vector of
   voice → note maps per column; `rnd` adds noise for variety."
  [key chords fixed rnd]
  (let [ctx {:key key :type :bass :chords chords :n (count chords)
             :infos (mapv (fn [c] {:chord c}) chords)}
        cands (vec (for [i (range (count chords))]
                     (vec (map #(update % :cost + (* 6 (rnd)))
                               (candidates ctx (chords i) i (get fixed i {}))))))]
    (when (every? seq cands)
      (let [step (fn [prev cs]
                   (vec (for [c cs]
                          (let [best (apply min-key :total
                                            (for [p prev]
                                              {:total (+ (:total p)
                                                         (cost-of ctx rules/check-pair pair-rules (:x p) (:x c))
                                                         (smooth (:col (:x p)) (:col (:x c))))
                                               :path (:path p)}))]
                            {:x (:x c)
                             :total (+ (:total best) (:cost c))
                             :path (conj (:path best) (:col (:x c)))}))))
            start (mapv (fn [c] {:x (:x c) :total (:cost c) :path [(:col (:x c))]}) (first cands))
            final (reduce step start (rest cands))
            best (apply min-key :total final)]
        {:cols (:path best) :cost (:total best)}))))
