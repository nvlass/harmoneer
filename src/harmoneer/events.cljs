(ns harmoneer.events
  (:require [re-frame.core :as rf]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.solver :as solver]
            [harmoneer.exercise :as ex]
            [harmoneer.audio :as audio]
            [harmoneer.storage :as storage]
            [harmoneer.midi :as midi]
            [harmoneer.i18n :as i18n]))

(def default-settings
  {:lang (i18n/browser-lang)
   :note-names (if (= :el (i18n/browser-lang)) :solfege :letters)
   :level 1
   :type :bass
   :key :random-major
   :bars 4
   :preset :strict
   :rules (rules/preset-rules :strict)
   :hear :chord
   :analysis true
   :midi false})

(def chord-s 1.1)

(defn writable [{:keys [given]}] (vec (remove #{given} h/voices)))

(def initial-db
  {:view :write
   :settings default-settings
   :loaded? false
   :samples nil
   :midi {:state :off :inputs []}
   :exercise nil
   :sel nil
   :history []
   :future []
   :focus nil
   :generating false
   :show-solution false
   :play {:token 0 :col nil :playing nil}
   :held #{}
   :chord-buf []
   :width 760})

;; ---------------------------------------------------------------- effects

(rf/reg-fx :audio/play (fn [events] (audio/stop-all!) (audio/play-events! events 4)))
(rf/reg-fx :audio/stop (fn [_] (audio/stop-all!)))
(rf/reg-fx :later
  (fn [xs] (doseq [{:keys [ms event]} xs] (js/setTimeout #(rf/dispatch event) ms))))
(rf/reg-fx :midi/connect (fn [on?] (if on? (midi/connect!) (midi/disconnect!))))
(rf/reg-fx :html-lang (fn [lang] (set! (.. js/document -documentElement -lang) (name lang))))

(def persisted [:settings :exercise])

(def persist
  "Saves the persisted parts of the db whenever they change, once storage has loaded."
  (rf/->interceptor
   :id :persist
   :after (fn [ctx]
            (let [before (get-in ctx [:coeffects :db])
                  after (get-in ctx [:effects :db])]
              (when (and after (:loaded? before))
                (doseq [k persisted
                        :when (not= (k before) (k after))]
                  (storage/save! k (k after)))))
            ctx)))

(rf/reg-global-interceptor persist)

;; ---------------------------------------------------------------- lifecycle

(rf/reg-event-db :init (fn [_ _] initial-db))

(rf/reg-event-fx :storage/loaded
  (fn [{:keys [db]} [_ {:keys [settings exercise]}]]
    (let [s (merge default-settings settings)
          s (if (= :custom (:preset s)) s (assoc s :rules (rules/preset-rules (:preset s))))]
      (cond-> {:db (cond-> (assoc db :settings s :loaded? true)
                     exercise (assoc :exercise exercise :sel [0 (first (writable exercise))]))
               :html-lang (:lang s)}
        (:midi s) (assoc :midi/connect true)))))

(rf/reg-event-db :audio/loaded (fn [db [_ n]] (assoc db :samples n)))

(rf/reg-event-db :set-view (fn [db [_ v]] (assoc db :view v)))

(rf/reg-event-db :resize (fn [db [_ w]] (assoc db :width w)))

;; ---------------------------------------------------------------- settings

(rf/reg-event-fx :settings/set
  (fn [{:keys [db]} [_ k v]]
    (cond-> {:db (assoc-in db [:settings k] v)}
      (= k :midi) (assoc :midi/connect v)
      (= k :lang) (assoc :html-lang v))))

(rf/reg-event-db :settings/preset
  (fn [db [_ p]] (update db :settings assoc :preset p :rules (rules/preset-rules p))))

(rf/reg-event-db :settings/toggle-rule
  (fn [db [_ id]]
    (update db :settings
            (fn [s]
              (let [rs ((if (contains? (:rules s) id) disj conj) (:rules s) id)
                    p (some (fn [p] (when (= rs (rules/preset-rules p)) p)) rules/presets)]
                (assoc s :rules rs :preset (or p :custom)))))))

;; ---------------------------------------------------------------- exercises

(defn- pick-key [choice]
  (if (map? choice)
    choice
    (let [modes (case choice :random-major [:major] :random-minor [:minor] [:major :minor])]
      (rand-nth (for [m modes k (t/keys-of m) :when (<= (Math/abs (t/signature k)) 4)] k)))))

(rf/reg-event-fx :new-exercise
  (fn [{:keys [db]} _]
    {:db (assoc db :generating true)
     :later [{:ms 30 :event [:new-exercise/run]}]}))

(rf/reg-event-fx :new-exercise/run
  (fn [{:keys [db]} _]
    (let [{:keys [level type bars] :as s} (:settings db)
          e (ex/generate rand {:key (pick-key (:key s)) :level level :type type :bars bars})]
      {:db (cond-> (assoc db :generating false)
             e (assoc :exercise e :sel [0 (first (writable e))] :history [] :future []
                      :focus nil :show-solution false))
       :audio/stop true})))

(rf/reg-event-db :toggle-solution (fn [db _] (update db :show-solution not)))

;; ---------------------------------------------------------------- editing

(defn- edit
  "Applies `f` to the written columns, keeping undo history."
  [db f]
  (let [before (get-in db [:exercise :cols])
        after (f before)]
    (if (= before after)
      db
      (-> db
          (assoc-in [:exercise :cols] after)
          (assoc :future [] :focus nil)
          (update :history #(vec (take-last 100 (conj % before))))))))

(defn- note-at [db [c v]] (get-in db [:exercise :cols c v]))

(defn- put [db [c v] n]
  (if (= v (get-in db [:exercise :given]))
    db
    (edit db #(if n (assoc-in % [c v] n) (update % c dissoc v)))))

(def centers {:s 33 :a 30 :t 26 :b 22})

(defn- ref-step [db [c v]]
  (let [cols (get-in db [:exercise :cols])]
    (or (some-> (get-in cols [c v]) :s)
        (some #(some-> (get-in cols [% v]) :s) (concat (range (dec c) -1 -1) (range (inc c) (count cols))))
        (centers v))))

(defn- nearest [key l ref]
  (let [s (first (filter #(= l (mod % 7)) (range (- ref 3) (+ ref 4))))]
    {:s s :a (t/scale-alt key l)}))

(defn- audition [db [c v]]
  (let [col (get-in db [:exercise :cols c])]
    (case (get-in db [:settings :hear])
      :off nil
      :note (when-let [n (col v)] [[(t/midi n) 0 1.2]])
      (seq (for [n (h/present col)] [(t/midi n) 0 1.2])))))

(defn- with-audition [db sel]
  (cond-> {:db db}
    (audition db sel) (assoc :audio/play (audition db sel))))

(defn- next-slot
  "The next writable voice down the column, then the first one of the next column."
  [e [c v]]
  (let [ws (writable e)
        later (rest (drop-while #(not= v %) ws))
        n (count (:cols e))]
    (if (seq later) [c (first later)] [(min (dec n) (inc c)) (first ws)])))

(rf/reg-event-fx :pick
  (fn [{:keys [db]} [_ c staff step]]
    (let [e (:exercise db)
          col (get-in e [:cols c])
          [sc sv] (:sel db)
          cands (filter (set (writable e)) (if (= staff :treble) [:s :a] [:t :b]))
          v (cond (= 1 (count cands)) (first cands)
                  (and (some #{sv} cands) (nil? (col sv))) sv
                  (some #(nil? (col %)) cands) (first (filter #(nil? (col %)) cands))
                  :else (apply min-key #(Math/abs (- step (:s (col %)))) cands))
          sel [c v]
          old (col v)
          db (cond-> (assoc db :sel sel)
               (not= step (:s old)) (put sel {:s step :a (t/scale-alt (:key e) (mod step 7))}))]
      (with-audition db sel))))

(rf/reg-event-db :select (fn [db [_ sel]] (assoc db :sel sel)))

(rf/reg-event-db :select-voice
  (fn [db [_ v]]
    (if (and (:exercise db) (not= v (get-in db [:exercise :given])))
      (assoc db :sel [(or (first (:sel db)) 0) v])
      db)))

(rf/reg-event-db :next-voice
  (fn [db _]
    (if-let [[c v] (:sel db)]
      (let [ws (writable (:exercise db))]
        (assoc db :sel [c (nth ws (mod (inc (.indexOf ws v)) (count ws)))]))
      db)))

(rf/reg-event-db :move-col
  (fn [db [_ d]]
    (if-let [[c v] (:sel db)]
      (assoc db :sel [(max 0 (min (dec (count (get-in db [:exercise :cols]))) (+ c d))) v])
      db)))

(defn- step-note [db sel delta]
  (let [key (get-in db [:exercise :key])
        n (or (note-at db sel) {:s (ref-step db sel) :a 0})
        s (+ (:s n) delta)
        a (if (zero? (mod delta 7)) (:a n) (t/scale-alt key (mod s 7)))]
    (put db sel {:s s :a a})))

(defn- alter-note [db sel f]
  (if-let [n (note-at db sel)]
    (put db sel (update n :a #(max -2 (min 2 (f %)))))
    db))

(rf/reg-event-fx :edit
  (fn [{:keys [db]} [_ op & args]]
    (if-let [sel (:sel db)]
      (let [after (case op
                    :up (step-note db sel (first args))
                    :raise (alter-note db sel inc)
                    :lower (alter-note db sel dec)
                    :natural (alter-note db sel (constantly 0))
                    :delete (put db sel nil)
                    db)]
        (if (= op :delete) {:db after} (with-audition after sel)))
      {:db db})))

(rf/reg-event-fx :letter
  (fn [{:keys [db]} [_ l]]
    (if-let [sel (:sel db)]
      (let [db (put db sel (nearest (get-in db [:exercise :key]) l (ref-step db sel)))]
        (update (with-audition db sel) :db assoc :sel (next-slot (:exercise db) sel)))
      {:db db})))

(rf/reg-event-db :backspace
  (fn [db _]
    (if-let [sel (:sel db)]
      (if (note-at db sel)
        (put db sel nil)
        (let [e (:exercise db)
              ws (writable e)
              [c v] sel
              prev (if (= v (first ws)) [(max 0 (dec c)) (peek ws)] [c (nth ws (dec (.indexOf ws v)))])]
          (-> db (put prev nil) (assoc :sel prev))))
      db)))

(rf/reg-event-db :undo
  (fn [db _]
    (if-let [prev (peek (:history db))]
      (-> db
          (update :future conj (get-in db [:exercise :cols]))
          (assoc-in [:exercise :cols] prev)
          (update :history pop)
          (assoc :focus nil))
      db)))

(rf/reg-event-db :redo
  (fn [db _]
    (if-let [nxt (peek (:future db))]
      (-> db
          (update :history conj (get-in db [:exercise :cols]))
          (assoc-in [:exercise :cols] nxt)
          (update :future pop)
          (assoc :focus nil))
      db)))

(rf/reg-event-db :clear
  (fn [db _]
    (let [given (get-in db [:exercise :given])]
      (-> (edit db (fn [cols] (mapv #(select-keys % [given]) cols)))
          (assoc :sel [0 (first (writable (:exercise db)))])))))

(rf/reg-event-db :focus
  (fn [db [_ i]] (assoc db :focus (when-not (= i (:focus db)) i))))

;; ---------------------------------------------------------------- hints

(defn fill
  "The selected column voiced to fit what is written around it, keeping its notes where possible."
  [{:keys [key chords cols given] :as e} c]
  (let [fixed (fn [keep-col?]
                (vec (for [[i col] (map-indexed vector cols)]
                       (cond (= i c) (if keep-col? col (select-keys col [given]))
                             (#{(dec c) (inc c)} i) col
                             :else (select-keys col [given])))))
        sol (or (solver/solve key chords (fixed true) (constantly 0))
                (solver/solve key chords (fixed false) (constantly 0))
                (solver/solve key chords (mapv #(select-keys % [given]) cols) (constantly 0)))]
    (when sol (get (:cols sol) c))))

(rf/reg-event-fx :fill
  (fn [{:keys [db]} _]
    (let [e (:exercise db)
          [c v] (:sel db)]
      (if-let [col (and e c (fill e c))]
        (with-audition (-> (edit db #(assoc % c col)) (assoc :sel [c v])) [c v])
        {:db db}))))

;; ---------------------------------------------------------------- playback

(rf/reg-event-fx :play
  (fn [{:keys [db]} [_ which]]
    (let [e (:exercise db)
          cols (if (= which :solution) (:solution e) (:cols e))
          n (count cols)
          token (inc (get-in db [:play :token]))
          events (for [[i col] (map-indexed vector cols)
                       note (h/present col)]
                   [(t/midi note) (* i chord-s) (if (= i (dec n)) (* 2 chord-s) (* 0.95 chord-s))])]
      (if (empty? events)
        {:db db}
        {:db (assoc db :play {:playing which :token token :col 0})
         :audio/play events
         :later (for [i (range 1 (inc n))]
                  {:ms (* 1000 chord-s (if (= i n) (inc i) i)) :event [:play-tick token which (when (< i n) i)]})}))))

(rf/reg-event-db :play-tick
  (fn [db [_ token which col]]
    (if (= token (get-in db [:play :token]))
      (assoc db :play {:playing (when col which) :token token :col col})
      db)))

(rf/reg-event-fx :stop
  (fn [{:keys [db]} _]
    {:db (update db :play #(assoc % :playing nil :col nil :token (inc (:token %))))
     :audio/stop true}))

(rf/reg-event-fx :toggle-play
  (fn [{:keys [db]} _]
    {:dispatch (if (get-in db [:play :playing]) [:stop] [:play :answer])}))

;; ---------------------------------------------------------------- midi

(rf/reg-event-db :midi/status (fn [db [_ s]] (assoc db :midi s)))

(rf/reg-event-db :midi/note-on
  (fn [db [_ m]] (-> db (update :held conj m) (update :chord-buf conj m))))

(defn- place-chord
  "Notes played together go to the writable voices from the top; a single note to the selected voice."
  [db ms]
  (let [e (:exercise db)
        key (:key e)
        [c v :as sel] (:sel db)
        ws (writable e)
        notes (map #(t/spell key %) (sort > (distinct ms)))
        notes (cond (= 4 (count notes)) (if (= :b (:given e)) (butlast notes) (rest notes))
                    :else notes)]
    (cond
      (= 1 (count notes))
      (let [db (put db sel (first notes))]
        (update (with-audition db sel) :db assoc :sel (next-slot e sel)))
      (= (count notes) (count ws))
      (let [db (reduce (fn [db [w n]] (put db [c w] n)) db (map vector ws notes))]
        (update (with-audition db sel) :db assoc :sel [(min (dec (count (:cols e))) (inc c)) (first ws)]))
      :else {:db db})))

(rf/reg-event-fx :midi/note-off
  (fn [{:keys [db]} [_ m]]
    (let [db (update db :held disj m)]
      (if (and (empty? (:held db)) (seq (:chord-buf db)))
        (let [buf (:chord-buf db)
              db (assoc db :chord-buf [])]
          (if (and (:sel db) (:exercise db) (= :write (:view db)))
            (place-chord db buf)
            {:db db}))
        {:db db}))))

;; ---------------------------------------------------------------- keyboard

(def letter-keys (zipmap ["c" "d" "e" "f" "g" "a" "b"] (range)))

(rf/reg-event-fx :key
  (fn [{:keys [db]} [_ k {:keys [shift mod]}]]
    (cond
      (and mod (= k "z")) {:dispatch (if shift [:redo] [:undo])}
      (and mod (= k "y")) {:dispatch [:redo]}
      mod {}
      (or (not= :write (:view db)) (nil? (:exercise db))) {}
      :else
      (if-let [ev (case k
                    "ArrowUp" [:edit :up (if shift 7 1)]
                    "ArrowDown" [:edit :up (if shift -7 -1)]
                    "ArrowLeft" [:move-col -1]
                    "ArrowRight" [:move-col 1]
                    "Tab" [:next-voice]
                    ("#" "+") [:edit :raise]
                    "-" [:edit :lower]
                    "n" [:edit :natural]
                    "Backspace" [:backspace]
                    "Delete" [:edit :delete]
                    " " [:toggle-play]
                    "h" [:fill]
                    (when-let [l (letter-keys (.toLowerCase k))] [:letter l]))]
        {:dispatch ev}
        {}))))
