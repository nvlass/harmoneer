(ns harmoneer.subs
  (:require [re-frame.core :as rf]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.i18n :as i18n]))

(doseq [k [:view :settings :exercise :sel :focus :samples :play :midi :generating :show-solution
           :history :future :width]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :loc :<- [:settings] (fn [s _] (i18n/locale (:lang s) (:note-names s))))

(rf/reg-sub :context :<- [:exercise] (fn [e _] (when e (rules/context e))))

(rf/reg-sub :violations
  :<- [:exercise] :<- [:settings]
  (fn [[e s] _] (if e (rules/analyze e (:rules s)) [])))

(rf/reg-sub :score :<- [:violations] (fn [vs _] (rules/score vs)))

(def rank {:error 2 :warning 1})

(rf/reg-sub :marks
  :<- [:violations]
  (fn [vs _]
    (reduce (fn [acc {:keys [cols voices level]}]
              (reduce (fn [acc slot] (update acc slot #(if (and % (>= (rank %) (rank level))) % level)))
                      acc
                      (if (seq voices) (for [c cols v voices] [c v]) (for [c cols] [c nil]))))
            {} vs)))

(defn figures
  "Figured-bass rows per column of a given-bass exercise."
  [{:keys [key chords cols type]}]
  (when (= type :bass)
    (vec (for [[i c] (map-indexed vector chords)
               :let [bass (get-in cols [i :b])
                     prev (get chords (dec i))
                     after-64? (and (= 2 (:inv prev)) (= 0 (:d prev)) (= 4 (:d c)))]]
           (h/figures key c bass after-64?)))))

(rf/reg-sub :figures :<- [:exercise] (fn [e _] (when e (figures e))))

(rf/reg-sub :numerals
  :<- [:context] :<- [:settings]
  (fn [[ctx s] _]
    (when (and ctx (:analysis s))
      (vec (for [{:keys [chord complete?]} (:infos ctx)]
             (when complete?
               (if chord
                 (str (h/numeral (:key ctx) chord) (h/inversion-figure (:inv chord)))
                 "?")))))))

(rf/reg-sub :solution-numerals
  :<- [:exercise]
  (fn [e _]
    (when e (mapv #(str (h/numeral (:key e) %) (h/inversion-figure (:inv %))) (:chords e)))))

(rf/reg-sub :filled
  :<- [:exercise]
  (fn [e _] (count (filter #(= 4 (count (h/present %))) (:cols e)))))
