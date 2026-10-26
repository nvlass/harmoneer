(ns harmoneer.score
  (:require [reagent.core :as r]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]))

;; A grand staff in systems of whole bars, two chords (half notes) to a bar and a whole note at the
;; end. Soprano and alto share the treble staff (stems up, down), tenor and bass the bass staff.
;; 5px per diatonic step, so staff lines are 10px apart. Figures and numerals go under the bass.

(def sp 5)
(def treble-top 38)
(def bass-top 26)
(def pad-top 44)
(def staff-gap 60)
(def treble-y pad-top)
(def bass-y (+ pad-top 40 staff-gap))
(def fig-y (+ bass-y 40 40))
(def height (+ fig-y 64))
(def stem-len 32)
(def col-w 50)

(def staff-of {:s :treble :a :treble :t :bass :b :bass})
(def stem-up? {:s true :a false :t true :b false})

(defn- y-of [staff step]
  (if (= staff :treble)
    (+ treble-y (* sp (- treble-top step)))
    (+ bass-y (* sp (- bass-top step)))))

(def sharp-steps {:treble [38 35 39 36 33 37 34] :bass [24 21 25 22 19 23 20]})
(def flat-steps {:treble [34 37 33 36 32 35 31] :bass [20 23 19 22 18 21 17]})

(defn x0 [keysig first?] (+ 40 (* 9 (Math/abs (t/signature keysig))) 10 (if first? 26 0)))

(defn bars-of [n-cols] (Math/ceil (/ n-cols 2)))

(defn system-width [keysig first? n-bars] (+ (x0 keysig first?) (* n-bars 2 col-w) 6))

(defn- ledger-steps [staff step]
  (let [top (if (= staff :treble) treble-top bass-top) bottom (- top 8)]
    (concat (when (>= step (+ top 2)) (range (+ top 2) (inc step) 2))
            (when (<= step (- bottom 2)) (range (- bottom 2) (dec step) -2)))))

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y whole?]
  (if whole?
    (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35))
    (str (ellipse-path x y 6.4 4.5 -22) (ellipse-path x y 5.4 2.1 -22))))

(defn- clef-glyph
  "Clef glyphs differ a lot between fonts, so measure the glyph and scale it into the given box."
  [_ _ _ _]
  (let [el (atom nil)
        bbox (r/atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [] (when-let [n @el]
               (let [b (.getBBox n)]
                 (when (pos? (.-height b)) (reset! bbox [(.-x b) (.-y b) (.-height b)])))))
      :reagent-render
      (fn [glyph x y-top hgt]
        (let [[bx by bh] @bbox]
          [:text.clef {:ref #(reset! el %) :x 0 :y 0 :font-size 40
                       :visibility (if bh "visible" "hidden")
                       :transform (when bh (str "translate(" x "," y-top ") scale(" (/ hgt bh) ") translate(" (- bx) "," (- by) ")"))}
           glyph]))})))

(defn- staff-el [staff keysig first? w]
  (let [y0 (if (= staff :treble) treble-y bass-y)
        sig (t/signature keysig)
        steps (take (Math/abs sig) ((if (neg? sig) flat-steps sharp-steps) staff))]
    [:g.staff
     (for [k (range 5)]
       ^{:key k} [:line {:x1 2 :x2 (- w 4) :y1 (+ y0 (* 10 k)) :y2 (+ y0 (* 10 k))}])
     (if (= staff :treble)
       [clef-glyph "𝄞" 6 (- y0 13) 68]
       [clef-glyph "𝄢" 7 (- y0 1) 32])
     (for [[k st] (map-indexed vector steps)]
       ^{:key k}
       [:text.acc {:x (+ 38 (* 9 k)) :y (+ (y-of staff st) 5)} (if (neg? sig) "♭" "♯")])
     (when first?
       (let [x (- (x0 keysig true) 16)]
         [:g.tsig
          [:text {:x x :y (+ y0 19) :text-anchor "middle"} 4]
          [:text {:x x :y (+ y0 39) :text-anchor "middle"} 4]]))]))

(defn bar-accidentals
  "[col voice] → accidental to print, per staff and bar: an accidental holds for the rest of the bar."
  [key cols b]
  (into {}
        (for [staff [:treble :bass]
              :let [vs (if (= staff :treble) [:s :a] [:t :b])
                    slots (for [c [(* 2 b) (inc (* 2 b))] v vs
                                :let [n (get-in cols [c v])] :when n]
                            [[c v] n])
                    accs (t/accidentals key (map second slots))]
              [slot acc] (map vector (map first slots) accs)
              :when acc]
          [slot acc])))

(defn- event-point [e]
  (let [svg (.-currentTarget e)
        rect (.getBoundingClientRect svg)
        vb (.. svg -viewBox -baseVal)
        k (/ (.-width vb) (.-width rect))]
    [(* k (- (.-clientX e) (.-left rect))) (* k (- (.-clientY e) (.-top rect)))]))

(defn- fig-text [{:keys [num acc]}]
  (str (when acc (if (zero? acc) "♮" (t/acc-sym acc))) num))

(defn- note-el [{:keys [x y staff step whole? up? acc acc-x cls]}]
  (let [sx (if up? (+ x 5.6) (- x 5.6))]
    [:g {:class (str "note " cls)}
     (for [st (ledger-steps staff step)]
       ^{:key st} [:line.ledger {:x1 (- x 10) :x2 (+ x 10) :y1 (y-of staff st) :y2 (y-of staff st)}])
     (when acc [:text.acc {:x acc-x :y (+ y 5) :text-anchor "end"} (if (zero? acc) "♮" (t/acc-sym acc))])
     [:path.head {:d (head-path x y whole?) :fill-rule "evenodd"}]
     (when-not whole?
       [:line.stem {:x1 sx :x2 sx :y1 (if up? (- y 1) (+ y 1)) :y2 (if up? (- y stem-len) (+ y stem-len))}])]))

(defn- column-notes
  "Note elements of column `c` at x: the upper voice of a second moves right, and accidentals of
   close notes on one staff stagger."
  [key cols c x accs cls whole?]
  (let [col (get cols c)]
    (for [[u l] [[:s :a] [:t :b]]
          v [u l]
          :let [n (col v)]
          :when n
          :let [other (col (if (= v u) l u))
                second? (and other (= 1 (- (:s (col u) -99) (:s (col l) 99))))
                nx (if (and second? (= v u)) (+ x 11) x)
                acc (accs [c v])
                both? (and (= v l) (accs [c u]) (col u) (< (- (:s (col u)) (:s n)) 6))
                staff (staff-of v)]]
      ^{:key (name v)}
      [note-el {:x nx :y (y-of staff (:s n)) :staff staff :step (:s n) :whole? whole?
                :up? (stem-up? v) :acc acc :acc-x (- x (if both? 19 10)) :cls (cls c v)}])))

(defn system
  "Columns [from, from + 2 × n-bars). `cols` are the written columns, `infos` the rules' view of them."
  [_]
  (let [hover (r/atom nil)]
    (fn [{:keys [keysig cols n-cols from n-bars first? sel marks focus playing given figures numerals
                 on-pick zoom solution?]}]
      (let [key keysig
            w (system-width key first? n-bars)
            x-start (x0 key first?)
            cx (fn [c] (+ x-start (* col-w (- c from)) (/ col-w 2) 4))
            to (min n-cols (+ from (* 2 n-bars)))
            pick (fn [e]
                   (let [[x y] (event-point e)
                         c (+ from (Math/floor (/ (- x x-start) col-w)))]
                     (when (and (<= from c) (< c to))
                       (let [staff (if (> y (/ (+ treble-y 40 bass-y) 2)) :bass :treble)
                             top (if (= staff :treble) treble-top bass-top)
                             y0 (if (= staff :treble) treble-y bass-y)
                             step (+ top (Math/round (/ (- y0 y) sp)))]
                         {:c c :staff staff :step (max (if (= staff :treble) 22 12) (min (if (= staff :treble) 44 32) step))}))))
            cls (fn [c v]
                  (str (some-> (get marks [c v]) name)
                       (when (= v given) " given")
                       (when (= sel [c v]) " sel")
                       (when (= playing c) " playing")))]
        [:svg.score {:viewBox (str "0 0 " w " " height) :width w :height height
                     :style {:width (* (or zoom 1) w)}
                     :class (when on-pick "interactive")
                     :on-pointer-move (when on-pick #(reset! hover (pick %)))
                     :on-pointer-leave #(reset! hover nil)
                     :on-click (when on-pick #(when-let [{:keys [c staff step]} (pick %)] (on-pick c staff step)))}
         (for [c (range from to)
               :let [x (- (cx c) (/ col-w 2) -2)]]
           ^{:key (str "bg" c)}
           [:rect {:class (str "colbg"
                               (when (= c (first sel)) " cur")
                               (when (some #{c} focus) " focus")
                               (some->> (get marks [c nil]) name (str " "))
                               (when (= playing c) " playing"))
                   :x x :y 6 :width (- col-w 4) :height (- height 12) :rx 6}])
         [staff-el :treble key first? w]
         [staff-el :bass key first? w]
         [:line {:x1 2 :x2 2 :y1 treble-y :y2 (+ bass-y 40)}]
         (for [b (range 1 (inc n-bars))
               :let [x (+ x-start (* b 2 col-w)) final? (and (= b n-bars) (>= (+ from (* 2 n-bars)) n-cols))]]
           ^{:key b}
           [:g {:class (if final? "bar final" "bar")}
            [:line {:x1 x :x2 x :y1 treble-y :y2 (+ bass-y 40)}]])
         (for [b (range (/ from 2) (+ (/ from 2) n-bars))
               :let [accs (bar-accidentals key cols b)]
               c [(* 2 b) (inc (* 2 b))]
               :when (and (< c to) (get cols c))]
           ^{:key (str "c" c)}
           [:g (column-notes key cols c (cx c) accs cls (= c (dec n-cols)))])
         (for [c (range from to)
               :let [fs (get figures c)]
               :when (seq fs)]
           ^{:key (str "f" c)}
           [:g.figures
            (for [[k f] (map-indexed vector fs)]
              ^{:key k} [:text {:x (cx c) :y (+ fig-y (* 14 k)) :text-anchor "middle"} (fig-text f)])])
         (for [c (range from to)
               :let [nm (get numerals c)]
               :when nm]
           ^{:key (str "r" c)}
           [:text.numeral {:x (cx c) :y (+ fig-y 44) :text-anchor "middle" :class (when (= nm "?") "bad")} nm])
         (when-let [{:keys [c staff step]} (and on-pick (not solution?) @hover)]
           (let [x (cx c) y (y-of staff step)]
             [:g.note.ghost
              (for [st (ledger-steps staff step)]
                ^{:key st} [:line.ledger {:x1 (- x 10) :x2 (+ x 10) :y1 (y-of staff st) :y2 (y-of staff st)}])
              [:path.head {:d (head-path x y (= c (dec n-cols))) :fill-rule "evenodd"}]]))]))))

(defn score
  "Wraps the bars into systems of `per-line` bars."
  [{:keys [n-cols per-line] :as props}]
  (let [n-bars (bars-of n-cols)]
    [:div.systems
     (for [b (range 0 n-bars per-line)]
       ^{:key b}
       [system (assoc props :from (* 2 b) :n-bars (min per-line (- n-bars b)) :first? (zero? b))])]))
