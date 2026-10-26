(ns harmoneer.examples-test
  (:require [cljs.test :refer [deftest is testing]]
            [harmoneer.examples :as ex]))

(deftest examples-break-what-they-say
  (doseq [[id {:keys [key chords expect]}] ex/all]
    (testing (name id)
      (let [vs (ex/violations key chords)]
        (is (= expect (set (map :rule vs))) (pr-str (map (juxt :rule :cols :voices) vs)))))))
