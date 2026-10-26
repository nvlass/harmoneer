(ns harmoneer.cheatsheet
  (:require [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.examples :as ex]
            [harmoneer.score :as score]
            [harmoneer.i18n :refer [tr tr-msg]]))

(defn- numerals [key cols]
  (vec (for [{:keys [chord]} (:infos (rules/context {:key key :type :soprano :cols cols}))]
         (if chord (str (h/numeral key chord) (h/inversion-figure (:inv chord))) "?"))))

(defn- marks [vs]
  (reduce (fn [acc {:keys [cols voices level]}]
            (reduce #(update %1 %2 (fn [l] (if (= l :error) l level))) acc
                    (for [c cols v (if (seq voices) voices [nil])] [c v])))
          {} vs))

(defn example [id]
  (let [{:keys [key chords]} (ex/all id)
        cols (mapv ex/parse-col chords)
        vs (ex/violations key chords)]
    [:figure.example
     [score/score {:keysig key :cols cols :n-cols (count cols) :per-line 8 :compact true
                   :marks (marks vs) :numerals (numerals key cols)}]
     [:figcaption (tr (keyword "cheat" (str "ex-" (name id))))
      (when (seq vs)
        [:ul.ex-faults (for [[i v] (map-indexed vector vs)] ^{:key i} [:li {:class (name (:level v))} (tr-msg (:msg v))])])]]))

(defn examples [& ids]
  [:div.examples (for [id ids] ^{:key id} [example id])])

(def functions {0 :t 1 :s 2 :t 3 :s 4 :d 5 :t 6 :d})

(defn degrees-table []
  [:table.degrees
   [:thead [:tr [:th ""] [:th (tr :cheat/th-major)] [:th (tr :cheat/th-minor)] [:th (tr :cheat/th-function)]]]
   [:tbody
    (for [d (range 7)
          :let [cell (fn [key]
                       (let [c {:d d :inv 0}]
                         [:td [:b (h/numeral key c)] " "
                          (interpose " " (for [n (h/tones key c)] (tr :fmt/pc {:s (:l n) :a (:a n)})))
                          [:small " " (tr (keyword "cheat" (str "q-" (name (h/quality key c)))))]]))]]
      ^{:key d}
      [:tr [:td.num (inc d)] (cell ex/c-major) (cell ex/a-minor)
       [:td (tr (keyword "cheat" (str "fn-" (name (functions d)))))]])]])

(defn rule-tags [{:keys [level presets types]}]
  [:<>
   [:span.ltag {:class (name level)} (tr (keyword "cheat" (str "level-" (name level))))]
   (when-not (contains? presets :relaxed) [:span.ltag.preset (tr :cheat/strict-only)])
   (when-not (contains? presets :strict) [:span.ltag.preset (tr :cheat/relaxed-only)])
   (when types [:span.ltag.preset (tr :cheat/melody-only)])])

(defn view []
  [:div.cheat
   [:div.card
    [:h3 (tr :cheat/voices-title)]
    (tr :cheat/voices)
    [examples :close :open]]

   [:div.card
    [:h3 (tr :cheat/degrees-title)]
    (tr :cheat/degrees)
    [degrees-table]]

   [:div.card
    [:h3 (tr :cheat/doubling-title)]
    (tr :cheat/doubling)
    [examples :double-root :double-third]]

   [:div.card
    [:h3 (tr :cheat/connect-title)]
    (tr :cheat/connect)
    [examples :common-tone :no-common :deceptive]]

   [:div.card
    [:h3 (tr :cheat/forbidden-title)]
    (tr :cheat/forbidden)
    [examples :parallels :hidden]]

   [:div.card
    [:h3 (tr :cheat/leading-title)]
    (tr :cheat/leading)
    [examples :minor :leading-tone]]

   [:div.card
    [:h3 (tr :cheat/inversions-title)]
    (tr :cheat/inversions)
    [examples :inversions]]

   [:div.card
    [:h3 (tr :cheat/six-four-title)]
    (tr :cheat/six-four)
    [examples :cadential :passing :pedal]]

   [:div.card
    [:h3 (tr :cheat/cadences-title)]
    (tr :cheat/cadences)
    [examples :perfect :plagal :half]]

   [:div.card
    [:h3 (tr :cheat/melody-title)]
    (tr :cheat/melody)]

   [:div.card
    [:h3 (tr :cheat/rules-title)]
    (tr :cheat/rules-hint)
    (for [g rules/groups]
      ^{:key g}
      [:div.rule-group
       [:div.group-label (tr (keyword "group" (name g)))]
       [:ul.rule-list
        (for [{:keys [id] :as r} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:li [rule-tags r] " " [:b (tr (keyword "rule" (name id)))]
           [:div (tr (keyword "rule.desc" (name id)))]
           [:div.fix (tr :ui/try) (tr (keyword "fix" (name id)))]])]])]

   [:div.card
    [:h3 (tr :cheat/about-title)]
    (tr :cheat/about (rules/penalty :error) (rules/penalty :warning))]])
