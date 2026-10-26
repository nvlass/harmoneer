(ns harmoneer.views
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.score :as score]
            [harmoneer.cheatsheet :as cheat]
            [harmoneer.events :as events]
            [harmoneer.i18n :as i18n :refer [tr tr-msg]]))

(defn <sub [q] @(rf/subscribe q))
(defn >evt [e] (rf/dispatch e))

(defn seg [options value on-change]
  [:div.seg
   (for [[v label & [disabled]] options]
     ^{:key (str v)}
     [:button {:class (when (= v value) "on") :disabled disabled :on-click #(on-change v)} label])])

(defn toggle [label value on-change & [hint]]
  [:label.toggle
   [:input {:type "checkbox" :checked (boolean value) :on-change #(on-change (not value))}]
   [:span label (when hint [:small hint])]])

(defn field [label & body] (into [:div.field [:span label]] body))

(defn- setter [k] #(>evt [:settings/set k %]))

(defn- zoom [] (if (> (<sub [:width]) 700) 1.2 1))

(defn- per-line
  "Bars per line: as many as fit, spread evenly over the lines."
  [key n-bars]
  (let [avail (/ (- (min (<sub [:width]) 960) 56) (zoom))
        fit (max 1 (Math/floor (/ (- avail (score/x0 key true) 6) (* 2 score/col-w))))
        lines (Math/ceil (/ n-bars fit))]
    (Math/ceil (/ n-bars lines))))

;; ---------------------------------------------------------------- setup

(defn- key-value [k]
  (if (map? k) (str (get-in k [:tonic :l]) "/" (get-in k [:tonic :a]) "/" (name (:mode k))) (name k)))

(defn key-select []
  (let [k (:key (<sub [:settings]))
        opts (concat [[:random-major (tr :ui/random-major)] [:random-minor (tr :ui/random-minor)] [:random-any (tr :ui/random-any)]]
                     (for [m [:major :minor] key (t/keys-of m)] [key (tr :fmt/key key)]))
        by-value (into {} (for [[v _] opts] [(key-value v) v]))]
    [:select {:value (key-value k) :on-change #(>evt [:settings/set :key (by-value (.. % -target -value))])}
     (for [[v label] opts]
       ^{:key (key-value v)} [:option {:value (key-value v)} label])]))

(defn setup-card []
  (let [s (<sub [:settings])]
    [:div.card.setup
     [field (tr :ui/level)
      [seg (for [l (range 1 (inc h/max-level))] [l (str l)]) (:level s) (setter :level)]
      [:span.hint [:b (tr (keyword "level" (str (:level s))))] " · " (tr (keyword "level.desc" (str (:level s))))]]
     [:div.row2
      [field (tr :ui/type)
       [seg [[:bass (tr :ui/type-bass)] [:soprano (tr :ui/type-soprano)]] (:type s) (setter :type)]]
      [field (tr :ui/bars)
       [seg (for [b [4 6 8]] [b (str b)]) (:bars s) (setter :bars)]]]
     [:div.row2
      [field (tr :ui/key) [key-select]]
      [:button.primary.new {:disabled (<sub [:generating]) :on-click #(>evt [:new-exercise])}
       (if (<sub [:generating]) (tr :ui/generating) (tr :ui/new))]]]))

;; ---------------------------------------------------------------- exercise

(defn toolbar []
  (let [{:keys [given] :as e} (<sub [:exercise])
        [c v :as sel] (<sub [:sel])
        note (when sel (get-in e [:cols c v]))]
    [:div.toolbar
     [:div.voices
      (for [w h/voices]
        ^{:key w}
        [:button {:class (str (when (= w v) "on") (when (= w given) " given"))
                  :disabled (= w given)
                  :title (tr :fmt/voice w)
                  :on-click #(>evt [:select-voice w])}
         (tr :fmt/voice-abbr w)])]
     [:span.sel-info (if sel (tr :ui/sel-info c v note) (tr :ui/click-staff))]
     [:div.tools
      [:button {:title (tr :ui/tool-up) :on-click #(>evt [:edit :up 1])} "↑"]
      [:button {:title (tr :ui/tool-down) :on-click #(>evt [:edit :up -1])} "↓"]
      [:button {:title (tr :ui/tool-sharp) :disabled (not note) :on-click #(>evt [:edit :raise])} "♯"]
      [:button {:title (tr :ui/tool-flat) :disabled (not note) :on-click #(>evt [:edit :lower])} "♭"]
      [:button {:title (tr :ui/tool-natural) :disabled (not note) :on-click #(>evt [:edit :natural])} "♮"]
      [:button {:title (tr :ui/tool-delete) :on-click #(>evt [:backspace])} "⌫"]
      [:button {:title (tr :ui/tool-undo) :disabled (empty? (<sub [:history])) :on-click #(>evt [:undo])} "↶"]
      [:button {:title (tr :ui/tool-redo) :disabled (empty? (<sub [:future])) :on-click #(>evt [:redo])} "↷"]
      [:button.hint-btn {:title (tr :ui/fill-title) :disabled (not sel) :on-click #(>evt [:fill])} (tr :ui/fill)]]]))

(defn exercise-card []
  (let [{:keys [key level type cols given] :as e} (<sub [:exercise])
        vs (<sub [:violations])
        focus (<sub [:focus])
        {:keys [playing col]} (<sub [:play])
        n-bars (score/bars-of (count cols))]
    [:div.card.score-card
     [:div.ex-head
      [:h3 (tr :fmt/key key) [:small " · " (tr :ui/level) " " level " · " (tr (if (= type :bass) :ui/type-bass :ui/type-soprano))]]
      [:p.hint (tr (if (= type :bass) :ui/task-bass :ui/task-soprano))]]
     [toolbar]
     [:div.score-wrap
      [score/score {:keysig key :cols cols :n-cols (count cols)
                    :per-line (per-line key n-bars) :zoom (zoom)
                    :sel (<sub [:sel]) :given given
                    :marks (<sub [:marks])
                    :focus (when focus (:cols (get vs focus)))
                    :playing (when (= playing :answer) col)
                    :figures (<sub [:figures])
                    :numerals (<sub [:numerals])
                    :on-pick (fn [c staff step] (>evt [:pick c staff step]))}]]
     [:div.play-bar
      (if playing
        [:button.primary {:on-click #(>evt [:stop])} (tr :ui/stop)]
        [:button.primary {:on-click #(>evt [:play :answer])} (tr :ui/play)])
      [:button {:on-click #(>evt [:toggle-solution])}
       (tr (if (<sub [:show-solution]) :ui/hide-solution :ui/solution))]
      [:button.ghost {:on-click #(>evt [:clear])} (tr :ui/clear)]]]))

(defn solution-card []
  (when (<sub [:show-solution])
    (let [{:keys [key solution]} (<sub [:exercise])
          {:keys [playing col]} (<sub [:play])
          n-bars (score/bars-of (count solution))]
      [:div.card.solution
       [:h3 (tr :ui/solution-title)
        [:button.link {:on-click #(>evt (if (= playing :solution) [:stop] [:play :solution]))}
         (tr (if (= playing :solution) :ui/stop :ui/play))]]
       [:div.score-wrap
        [score/score {:keysig key :cols solution :n-cols (count solution)
                      :per-line (per-line key n-bars) :zoom (zoom)
                      :playing (when (= playing :solution) col)
                      :numerals (<sub [:solution-numerals])
                      :solution? true}]]
       [:p.hint (tr :ui/solution-hint)]])))

(defn violation-row [i {:keys [rule level cols msg]} focus]
  [:li {:class (str (name level) (when (= i focus) " focus"))
        :on-click #(>evt [:focus i])}
   [:span.dot]
   [:div.v-body
    [:div.v-msg (tr-msg msg)]
    [:div.v-meta (tr :ui/cols cols) " · " (tr (keyword "rule" (name rule)))]
    (when (= i focus)
      [:<>
       [:div.v-fix (tr (keyword "rule.desc" (name rule)))]
       [:div.v-fix [:b (tr :ui/try)] (tr (keyword "fix" (name rule)))]])]
   [:button.link {:title (tr :ui/mute-title)
                  :on-click (fn [e] (.stopPropagation e) (>evt [:settings/toggle-rule rule]))} (tr :ui/mute)]])

(defn results-card []
  (let [vs (<sub [:violations])
        {:keys [errors warnings penalty]} (<sub [:score])
        n (count (:cols (<sub [:exercise])))
        filled (<sub [:filled])
        focus (<sub [:focus])]
    [:div.card.results
     [:div.summary
      [:div
       [:h3 (tr :ui/rule-check)]
       [:div.hint (if (< filled n) (tr :ui/progress filled n) (tr :ui/complete))]]
      [:div.penalty {:class (cond (pos? errors) "bad" (pos? warnings) "warn" :else "good")}
       [:b penalty] [:small (tr :ui/penalty)]]]
     (cond
       (and (empty? vs) (= filled n)) [:p.clean.done (tr :ui/done)]
       (empty? vs) [:p.clean (tr :ui/no-violations)]
       :else [:ul.violations
              (for [[i v] (map-indexed vector vs)]
                ^{:key i} [violation-row i v focus])])
     [:p.hint (tr :ui/results-hint (rules/penalty :error) (rules/penalty :warning))]]))

(defn write-view []
  (let [e (<sub [:exercise])]
    [:<>
     [setup-card]
     (if e
       [:<>
        [exercise-card]
        [results-card]
        [solution-card]
        [:p.hint.keys (tr :ui/keys-hint)]]
       [:div.card [:p.hint (tr :ui/no-exercise)]])]))

;; ---------------------------------------------------------------- rules & settings

(defn rules-view []
  (let [{:keys [preset midi note-names hear analysis lang] :as s} (<sub [:settings])
        enabled (:rules s)
        midi-state (<sub [:midi])
        names (i18n/solfege lang)]
    [:<>
     [:div.card
      [:h3 (tr :ui/rule-set)]
      [seg (conj (mapv (fn [p] [p (tr (keyword "preset" (name p)))]) rules/presets)
                 [:custom (tr :preset/custom) (not= preset :custom)])
       preset #(>evt [:settings/preset %])]
      [:p.hint (tr :ui/rule-set-hint)]]
     (for [g rules/groups]
       ^{:key g}
       [:div.card
        [:h3 (tr (keyword "group" (name g)))]
        (for [{:keys [id] :as r} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:label.toggle.rule
           [:input {:type "checkbox" :checked (contains? enabled id)
                    :on-change #(>evt [:settings/toggle-rule id])}]
           [:span [cheat/rule-tags r] " " (tr (keyword "rule" (name id)))
            [:small (tr (keyword "rule.desc" (name id)))]]])])
     [:div.card
      [:h3 (tr :ui/settings)]
      [field (tr :ui/hear)
       [seg [[:chord (tr :ui/hear-chord)] [:note (tr :ui/hear-note)] [:off (tr :ui/hear-off)]] hear (setter :hear)]]
      [field (tr :ui/note-names)
       [seg [[:letters "C D E"] [:solfege (str/join " " (take 3 names))]] note-names (setter :note-names)]]
      [toggle (tr :ui/analysis) analysis (setter :analysis)]
      [toggle (tr :ui/midi-toggle) midi (setter :midi)
       (case (:state midi-state)
         :ready (if (seq (:inputs midi-state))
                  (tr :ui/midi-connected (:inputs midi-state))
                  (tr :ui/midi-none))
         :unsupported (tr :ui/midi-unsupported)
         :denied (tr :ui/midi-denied)
         (tr :ui/midi-off))]]]))

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])
        s (<sub [:settings])]
    [:div.app
     [:header
      [:h1 "Harmoneer"]
      [:div.header-end
       (when-not (<sub [:samples]) [:span.badge (tr :ui/loading-sounds)])
       [:div.lang {:title (tr :ui/language)}
        [seg [[:en "EN"] [:el "ΕΛ"]] (:lang s) (setter :lang)]]]]
     [:nav
      (for [v [:write :cheat :rules]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} (tr (keyword "nav" (name v)))])]
     (case view
       :rules [rules-view]
       :cheat [cheat/view]
       [write-view])]))
