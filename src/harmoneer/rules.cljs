(ns harmoneer.rules
  (:require [harmoneer.theory :as t]
            [harmoneer.harmony :as h]))

;; Every rule is checked; presets decide which ones are shown and counted, as in Counterpointer.
;; Rules check one column (check-col), two neighbouring columns (check-pair) or the whole
;; progression (check-all). A violation is {:cols [i ..] :voices [v ..] :msg [:v/<key> & args]}.
;; :types limits a rule to given-bass or given-soprano exercises.

(def rules
  [{:id :chord :group :chord :level :error :presets #{:strict :relaxed}}
   {:id :vocab :group :chord :level :warning :presets #{:strict :relaxed} :types #{:soprano}}
   {:id :complete :group :chord :level :error :presets #{:strict :relaxed}}
   {:id :fifth :group :chord :level :warning :presets #{:strict}}
   {:id :double-lt :group :chord :level :error :presets #{:strict :relaxed}}
   {:id :doubling :group :chord :level :warning :presets #{:strict}}
   {:id :doubling-third :group :chord :level :warning :presets #{:relaxed}}
   {:id :crossing :group :chord :level :error :presets #{:strict :relaxed}}
   {:id :spacing :group :chord :level :warning :presets #{:strict :relaxed}}
   {:id :range :group :chord :level :warning :presets #{:strict :relaxed}}

   {:id :parallel :group :motion :level :error :presets #{:strict :relaxed}}
   {:id :antiparallel :group :motion :level :warning :presets #{:strict :relaxed}}
   {:id :hidden :group :motion :level :warning :presets #{:strict}}
   {:id :hidden-leap :group :motion :level :warning :presets #{:relaxed}}
   {:id :overlap :group :motion :level :warning :presets #{:strict :relaxed}}
   {:id :common-tone :group :motion :level :warning :presets #{:strict}}
   {:id :contrary :group :motion :level :warning :presets #{:strict}}

   {:id :leading-tone :group :melody :level :error :presets #{:strict}}
   {:id :leading-tone-outer :group :melody :level :error :presets #{:relaxed}}
   {:id :melodic :group :melody :level :error :presets #{:strict :relaxed}}
   {:id :inner-leap :group :melody :level :warning :presets #{:strict}}

   {:id :six-four :group :progression :level :error :presets #{:strict :relaxed}}
   {:id :progression :group :progression :level :warning :presets #{:strict :relaxed} :types #{:soprano}}
   {:id :harmonic-rhythm :group :progression :level :warning :presets #{:strict} :types #{:soprano}}])

(def rule-by-id (into {} (map (juxt :id identity) rules)))

(def groups [:chord :motion :melody :progression])

(def presets [:strict :relaxed])

(defn preset-rules [p] (set (keep #(when (contains? (:presets %) p) (:id %)) rules)))

(def penalty {:error 10 :warning 3})

;; ---------------------------------------------------------------- context

(defn info
  "What the rules need to know about column `i`: its chord (the given one for a figured bass,
   otherwise what the notes spell once all four are written) and each voice's role in it."
  [key type chords i col]
  (let [complete? (= 4 (count (h/present col)))
        chord (if (= type :bass)
                (get chords i)
                (when complete? (h/identify key col)))
        roles (when chord (h/roles key chord col))]
    {:i i :col col :chord chord :roles roles :complete? complete?
     :clean? (and complete? chord (every? some? (vals roles)))}))

(defn context [{:keys [key type chords cols level]}]
  {:key key :type type :level level :n (count cols)
   :infos (vec (map-indexed #(info key type chords %1 %2) cols))})

(defn- m [n] (when n (t/midi n)))

;; ---------------------------------------------------------------- one column

(defmulti check-col (fn [id _ _] id))
(defmethod check-col :default [_ _ _] nil)

(defmethod check-col :chord [_ {:keys [key type]} {:keys [i col chord roles complete?]}]
  (if (= type :bass)
    (let [bad (filterv #(and (col %) (nil? (roles %))) h/upper)]
      (when (seq bad)
        [{:cols [i] :voices bad :msg [:v/chord key chord (mapv col bad)]}]))
    (when (and complete? (nil? chord))
      [{:cols [i] :voices h/voices :msg [:v/no-chord key]}])))

(defmethod check-col :vocab [_ {:keys [key level]} {:keys [i chord]}]
  (when (and chord (not (h/allowed? key level chord)))
    [{:cols [i] :voices [] :msg [:v/vocab key chord level]}]))

(defmethod check-col :complete [_ _ {:keys [i roles clean?]}]
  (when clean?
    (for [r [0 1] :when (not (some #{r} (vals roles)))]
      {:cols [i] :voices [] :msg [:v/missing r]})))

(defmethod check-col :fifth [_ {:keys [n]} {:keys [i roles clean?]}]
  (when (and clean? (< i (dec n)) (not (some #{2} (vals roles))))
    [{:cols [i] :voices [] :msg [:v/fifth]}]))

(defmethod check-col :double-lt [_ {:keys [key]} {:keys [i col]}]
  (let [vs (filterv #(some->> (col %) (h/leading-tone? key)) h/voices)]
    (when (> (count vs) 1)
      [{:cols [i] :voices vs :msg [:v/double-lt]}])))

(defn- doubled-voices [roles r] (filterv #(= r (roles %)) h/voices))

(defn- bad-doubling [key infos {:keys [i col chord roles clean?]}]
  (when clean?
    (let [lt? (fn [r] (some #(and (= r (roles %)) (h/leading-tone? key (col %))) h/voices))
          ok (h/good-doublings key chord (:chord (get infos (dec i))))]
      (first (for [r (h/doubled roles) :when (and (not (ok r)) (not (lt? r)))] r)))))

(defmethod check-col :doubling [_ {:keys [key infos]} {:keys [i chord roles] :as x}]
  (when-let [r (bad-doubling key infos x)]
    [{:cols [i] :voices (doubled-voices roles r) :msg [:v/doubling key chord r]}]))

(defmethod check-col :doubling-third [_ {:keys [key infos]} {:keys [i chord roles] :as x}]
  (when (and (= 1 (bad-doubling key infos x)) (= :major (h/quality key chord)))
    [{:cols [i] :voices (doubled-voices roles 1) :msg [:v/doubling key chord 1]}]))

(defmethod check-col :crossing [_ _ {:keys [i col]}]
  (for [[u l] (partition 2 1 h/voices)
        :when (and (col u) (col l) (< (m (col u)) (m (col l))))]
    {:cols [i] :voices [u l] :msg [:v/crossing u l]}))

(defmethod check-col :spacing [_ _ {:keys [i col]}]
  (for [[u l] [[:s :a] [:a :t]]
        :when (and (col u) (col l) (> (- (m (col u)) (m (col l))) 12))]
    {:cols [i] :voices [u l] :msg [:v/spacing u l]}))

(defmethod check-col :range [_ _ {:keys [i col]}]
  (for [v h/voices
        :let [n (col v) [lo hi] (h/ranges v)]
        :when (and n (not (<= lo (m n) hi)))]
    {:cols [i] :voices [v] :msg [:v/range v (> (m n) hi)]}))

;; ---------------------------------------------------------------- two columns

(defn- move [x y v] (when (and (x v) (y v)) (- (m (y v)) (m (x v)))))

(defn- perfect [lo hi]
  (let [iv (t/interval lo hi)]
    (when (and (= :P (:q iv)) (#{0 4} (:g7 iv))) (:g7 iv))))

(defn- outer-pair? [u l] (= [u l] [:s :b]))

(defmulti check-pair (fn [id _ _ _] id))
(defmethod check-pair :default [_ _ _ _] nil)

(defn- perfect-motions
  "Voice pairs moving from a perfect fifth or octave to the same one, with `ok?` on the two moves."
  [x y ok?]
  (let [a (:col x) b (:col y)]
    (for [[u l] h/pairs
          :when (every? some? [(a u) (a l) (b u) (b l)])
          :let [p1 (perfect (a l) (a u)) p2 (perfect (b l) (b u))
                mu (move a b u) ml (move a b l)]
          :when (and p1 (= p1 p2) (not (zero? mu)) (not (zero? ml)) (ok? mu ml))]
      [u l p1])))

(defmethod check-pair :parallel [_ _ x y]
  (for [[u l p] (perfect-motions x y #(= (pos? %1) (pos? %2)))]
    {:cols [(:i x) (:i y)] :voices [u l] :msg [:v/parallel u l p]}))

(defmethod check-pair :antiparallel [_ _ x y]
  (for [[u l p] (perfect-motions x y #(not= (pos? %1) (pos? %2)))]
    {:cols [(:i x) (:i y)] :voices [u l] :msg [:v/antiparallel u l p]}))

(defn- hidden [x y soprano-leap?]
  (let [a (:col x) b (:col y)]
    (when (every? some? [(a :s) (a :b) (b :s) (b :b)])
      (let [p2 (perfect (b :b) (b :s))
            ms (move a b :s) mb (move a b :b)]
        (when (and p2 (not= p2 (perfect (a :b) (a :s)))
                   (not (zero? ms)) (not (zero? mb)) (= (pos? ms) (pos? mb))
                   (or (not soprano-leap?) (> (:g (t/interval (a :s) (b :s))) 1)))
          [{:cols [(:i x) (:i y)] :voices [:s :b] :msg [:v/hidden p2]}])))))

(defmethod check-pair :hidden [_ _ x y] (hidden x y false))

(defmethod check-pair :hidden-leap [_ _ x y] (hidden x y true))

(defmethod check-pair :overlap [_ _ x y]
  (let [a (:col x) b (:col y)]
    (for [[u l] (partition 2 1 h/voices)
          :when (every? some? [(a u) (a l) (b u) (b l)])
          :when (or (< (m (b u)) (m (a l))) (> (m (b l)) (m (a u))))]
      {:cols [(:i x) (:i y)] :voices [u l] :msg [:v/overlap u l]})))

(defn- distinct-chords? [x y]
  (and (:chord x) (:chord y) (not= (:d (:chord x)) (:d (:chord y)))))

(defn- shared-letters [key x y]
  (let [lx (set (map :l (h/tones key (:chord x))))]
    (filter lx (map :l (h/tones key (:chord y))))))

(defn- root-positions? [x y] (= 0 (:inv (:chord x)) (:inv (:chord y))))

(defmethod check-pair :common-tone [_ {:keys [key]} x y]
  (when (and (:clean? x) (:clean? y) (distinct-chords? x y) (root-positions? x y))
    (let [shared (set (shared-letters key x y))
          a (:col x) b (:col y)
          held? (some #(and (= (a %) (b %)) (shared (t/letter (a %)))) h/upper)]
      (when (and (seq shared) (not held?))
        [{:cols [(:i x) (:i y)] :voices [] :msg [:v/common-tone]}]))))

(defn- resolves? [key a b v]
  (and (h/leading-tone? key (a v)) (= 1 (move a b v))))

(defmethod check-pair :contrary [_ {:keys [key]} x y]
  (when (and (:clean? x) (:clean? y) (distinct-chords? x y) (root-positions? x y)
             (empty? (shared-letters key x y)))
    (let [a (:col x) b (:col y)
          mb (move a b :b)
          bad (filterv #(let [mv (move a b %)]
                          (and (not (zero? mb)) (or (zero? mv) (= (pos? mv) (pos? mb)))
                               (not (resolves? key a b %))))
                       h/upper)]
      (when (seq bad)
        [{:cols [(:i x) (:i y)] :voices bad :msg [:v/contrary]}]))))

(defn- leading-tones [{:keys [key]} x y vs]
  (when (and (#{4 6} (:d (:chord x))) (#{0 5} (:d (:chord y))))
    (let [a (:col x) b (:col y)]
      (for [v vs
            :when (and (a v) (b v) (h/leading-tone? key (a v)) (not (resolves? key a b v)))]
        {:cols [(:i x) (:i y)] :voices [v] :msg [:v/leading-tone v]}))))

(defmethod check-pair :leading-tone [_ ctx x y] (leading-tones ctx x y h/voices))

(defmethod check-pair :leading-tone-outer [_ ctx x y] (leading-tones ctx x y [:s :b]))

(defmethod check-pair :melodic [_ _ x y]
  (let [a (:col x) b (:col y)]
    (for [v h/voices
          :when (and (a v) (b v))
          :let [iv (t/interval (a v) (b v))]
          :when (or (#{:A :AA} (:q iv)) (= 6 (:g7 iv)) (> (:g iv) 7))]
      {:cols [(:i x) (:i y)] :voices [v] :msg [:v/melodic v (assoc iv :dir (compare (m (b v)) (m (a v))))]})))

(defmethod check-pair :inner-leap [_ _ x y]
  (let [a (:col x) b (:col y)]
    (for [v [:a :t]
          :when (and (a v) (b v))
          :let [iv (t/interval (a v) (b v))]
          :when (and (>= (:g iv) 4) (<= (:g iv) 7) (not (#{:A :AA} (:q iv))) (not= 6 (:g7 iv)))]
      {:cols [(:i x) (:i y)] :voices [v] :msg [:v/inner-leap v iv]})))

;; ---------------------------------------------------------------- the progression

(defmulti check-all (fn [id _] id))
(defmethod check-all :default [_ _] nil)

(defn six-four-kind
  "How the 6/4 at `i` is used: :cadential, :passing, :pedal, or nil."
  [infos i]
  (let [x (get infos i) p (get infos (dec i)) q (get infos (inc i))
        b #(some-> % :col :b)
        same-bass? (fn [u v] (and (b u) (b v) (= (t/letter (b u)) (t/letter (b v)))))]
    (cond
      (and (= 0 (:d (:chord x))) (= 4 (:d (:chord q))) (= 0 (:inv (:chord q))) (same-bass? x q)) :cadential
      (and (b p) (b x) (b q)
           (let [i1 (t/interval (b p) (b x)) i2 (t/interval (b x) (b q))
                 d1 (compare (m (b x)) (m (b p))) d2 (compare (m (b q)) (m (b x)))]
             (and (= 1 (:g i1) (:g i2)) (not (zero? d1)) (= d1 d2))))
      :passing
      (and (b p) (b q) (= (m (b p)) (m (b x)) (m (b q))) (:chord p) (= (:chord p) (:chord q))) :pedal)))

(defmethod check-all :six-four [_ {:keys [infos]}]
  (for [{:keys [i chord]} infos
        :when (= 2 (:inv chord))
        :let [kind (six-four-kind infos i)]
        :when (or (nil? kind) (and (= kind :cadential) (odd? i)))]
    {:cols [i] :voices [:b] :msg [:v/six-four kind]}))

(defn retrogression? [x y]
  (and (#{4 6} (:d x)) (#{1 3} (:d y))))

(defmethod check-all :progression [_ {:keys [key infos]}]
  (for [[x y] (partition 2 1 infos)
        :when (and (:chord x) (:chord y) (retrogression? (:chord x) (:chord y)))]
    {:cols [(:i x) (:i y)] :voices [] :msg [:v/progression key (:chord x) (:chord y)]}))

(defmethod check-all :harmonic-rhythm [_ {:keys [infos]}]
  (for [[x y] (partition 2 1 infos)
        :when (and (odd? (:i x)) (:chord x) (:chord y) (= (:d (:chord x)) (:d (:chord y))))]
    {:cols [(:i x) (:i y)] :voices [] :msg [:v/harmonic-rhythm]}))

;; ---------------------------------------------------------------- analysis

(defn- tag [{:keys [id level]} vs]
  (for [v vs] (assoc v :rule id :level level)))

(defn active [enabled type]
  (filter #(and (contains? enabled (:id %)) (or (nil? (:types %)) (contains? (:types %) type))) rules))

(defn analyze
  "All violations of the `enabled` rules, ordered by column."
  [exercise enabled]
  (let [{:keys [infos type] :as ctx} (context exercise)
        rs (active enabled type)
        vs (concat
            (for [r rs x infos v (check-col (:id r) ctx x)] (first (tag r [v])))
            (for [r rs [x y] (partition 2 1 infos) v (check-pair (:id r) ctx x y)] (first (tag r [v])))
            (for [r rs v (check-all (:id r) ctx)] (first (tag r [v]))))]
    (vec (sort-by (fn [v] [(first (:cols v)) ({:error 0 :warning 1} (:level v))]) vs))))

(defn score [violations]
  (let [by (frequencies (map :level violations))]
    {:errors (by :error 0) :warnings (by :warning 0)
     :penalty (reduce + (map (comp penalty :level) violations))}))
