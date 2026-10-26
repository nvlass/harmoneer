(ns harmoneer.harmony-test
  (:require [cljs.test :refer [deftest is testing]]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]
            [harmoneer.exercise :as ex]))

(def C t/default-key)
(def a-minor {:tonic {:l 5 :a 0} :mode :minor})

(defn n [s & [a]] {:s s :a (or a 0)})

(deftest identify
  (is (= {:d 0 :inv 0} (h/identify C {:s (n 35) :a (n 32) :t (n 30) :b (n 21)})))
  (is (= {:d 4 :inv 1} (h/identify C {:s (n 32) :a (n 29) :t (n 25) :b (n 20)})))
  (is (= {:d 4 :inv 0} (h/identify a-minor {:s (n 34) :a (n 30) :t (n 25 1) :b (n 23)})))
  (is (nil? (h/identify C {:s (n 33) :a (n 32) :t (n 30) :b (n 21)}))))

(deftest figures
  (is (= [{:num nil :acc 1}] (h/figures a-minor {:d 4 :inv 0} (n 23) false)))
  (is (= [{:num 6 :acc nil}] (h/figures C {:d 0 :inv 1} (n 23) false)))
  (is (= [{:num 6 :acc nil} {:num 4 :acc nil}] (h/figures C {:d 0 :inv 2} (n 25) false)))
  (is (= [] (h/figures C {:d 0 :inv 0} (n 21) false))))

(defn- ex-cols [key chords cols] {:key key :type :bass :level 5 :chords chords :cols cols})

(deftest parallels
  (let [chords [{:d 0 :inv 0} {:d 1 :inv 0}]
        cols [{:s (n 35) :a (n 32) :t (n 28) :b (n 21)}
              {:s (n 36) :a (n 33) :t (n 29) :b (n 22)}]
        vs (rules/analyze (ex-cols C chords cols) (rules/preset-rules :strict))]
    (is (some #(= :parallel (:rule %)) vs))))

(def all-keys (concat (t/keys-of :major) (t/keys-of :minor)))

(deftest generated-solutions-are-clean
  (doseq [key all-keys level (range 1 (inc h/max-level)) type [:bass :soprano]]
    (let [e (ex/generate rand {:key key :level level :type type :bars 6})]
      (testing (str (t/tonic-name (:tonic key) t/letters) " " (:mode key) " level " level " " type)
        (is (some? e))
        (when e
          (let [vs (rules/analyze (assoc e :cols (:solution e)) (rules/preset-rules :strict))]
            (is (empty? (filter #(= :error (:level %)) vs)) (pr-str vs))
            (is (= 11 (count (:cols e))))))))))
