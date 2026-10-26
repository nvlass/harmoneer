(ns harmoneer.i18n-test
  (:require [cljs.test :refer [deftest is testing]]
            [harmoneer.i18n :as i18n]
            [harmoneer.i18n.en :as en]
            [harmoneer.i18n.el :as el]
            [harmoneer.rules :as rules]
            [harmoneer.exercise :as ex]
            [harmoneer.theory :as t]))

(defn- flat-keys [m prefix]
  (mapcat (fn [[k v]]
            (let [k (if prefix (keyword (str prefix "." (name k))) k)]
              (if (map? v) (flat-keys v (name k)) [k])))
          m))

(deftest same-keys
  (is (= (set (flat-keys en/dict nil)) (set (flat-keys el/dict nil)))))

(deftest every-rule-has-text
  (doseq [lang [:en :el] {:keys [id]} rules/rules
          k [(keyword "rule" (name id)) (keyword "rule.desc" (name id))]]
    (is (string? (i18n/t {:lang lang :names t/letters} k)) (str lang k))))

(deftest violations-render
  (let [e (ex/generate rand {:key {:tonic {:l 5 :a 0} :mode :minor} :level 5 :type :soprano :bars 4})
        ;; a deliberately bad answer: the solution shifted, with every rule on
        cols (mapv (fn [col] (into {} (for [[v n] col] [v (if (= v :s) n (update n :s + 1))]))) (:solution e))
        vs (rules/analyze (assoc e :cols cols) (set (map :id rules/rules)))]
    (is (seq vs))
    (doseq [lang [:en :el] v vs]
      (testing (str lang (:msg v))
        (is (string? (i18n/msg {:lang lang :names t/letters} (:msg v))))))))
