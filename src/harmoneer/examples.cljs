(ns harmoneer.examples
  (:require [clojure.string :as str]
            [harmoneer.harmony :as h]
            [harmoneer.rules :as rules]))

;; Short SATB examples for the cheat sheet, written as "S A T B" per chord, e.g. "C5 G4 E4 C3".
;; `expect` lists the rules each example should break, checked by examples-test.

(def c-major {:tonic {:l 0 :a 0} :mode :major})
(def a-minor {:tonic {:l 5 :a 0} :mode :minor})

(defn parse-note [s]
  (let [[_ l acc o] (re-matches #"([A-G])([#b]*)(-?\d)" s)]
    {:s (+ (str/index-of "CDEFGAB" l) (* 7 (js/parseInt o)))
     :a (reduce + (map {\# 1 \b -1} acc))}))

(defn parse-col [s] (zipmap h/voices (map parse-note (str/split s #"\s+"))))

(def checked
  "Rules the examples are checked with: the strict ones, minus those about a melody exercise."
  (disj (rules/preset-rules :strict) :vocab :progression :harmonic-rhythm))

(defn exercise [key chords]
  {:key key :type :soprano :level h/max-level :cols (mapv parse-col chords)})

(defn violations [key chords] (rules/analyze (exercise key chords) checked))

(def all
  {:close {:key c-major :chords ["C5 G4 E4 C3"] :expect #{}}
   :open {:key c-major :chords ["E5 G4 C4 C3"] :expect #{}}
   :double-root {:key c-major :chords ["C5 G4 E4 C3" "C5 A4 F4 F3" "B4 G4 D4 G3" "C5 G4 E4 C3"] :expect #{}}
   :double-third {:key c-major :chords ["E5 G4 E4 C3"] :expect #{:doubling}}
   :common-tone {:key c-major :chords ["G4 E4 C4 C3" "A4 F4 C4 F3" "G4 E4 C4 C3"] :expect #{}}
   :no-common {:key c-major :chords ["C5 A4 F4 F3" "B4 G4 D4 G3" "C5 G4 E4 C3"] :expect #{}}
   :deceptive {:key c-major :chords ["C5 G4 E4 C3" "B4 G4 D4 G3" "C5 E4 C4 A3"] :expect #{}}
   :parallels {:key c-major :chords ["C5 A4 F4 F3" "D5 B4 G4 G3"] :expect #{:parallel :contrary}}
   :hidden {:key c-major :chords ["E5 G4 C4 C3" "G5 G4 B3 G3"] :expect #{:hidden}}
   :inversions {:key c-major :chords ["C5 G4 E4 C3" "C5 G4 C4 E3" "C5 G4 E4 G3"] :expect #{:six-four}}
   :cadential {:key c-major :chords ["C5 G4 E4 C3" "C5 A4 F4 F3" "C5 G4 E4 G3" "B4 G4 D4 G3" "C5 G4 E4 C3"] :expect #{}}
   :passing {:key c-major :chords ["C5 G4 E4 C3" "B4 G4 D4 D3" "C5 G4 C4 E3"] :expect #{}}
   :pedal {:key c-major :chords ["G4 E4 C4 C3" "A4 F4 C4 C3" "G4 E4 C4 C3"] :expect #{}}
   :perfect {:key c-major :chords ["C5 A4 F4 F3" "B4 G4 D4 G3" "C5 G4 E4 C3"] :expect #{}}
   :plagal {:key c-major :chords ["C5 A4 F4 F3" "C5 G4 E4 C3"] :expect #{}}
   :half {:key c-major :chords ["C5 G4 E4 C3" "B4 G4 D4 G3"] :expect #{}}
   :minor {:key a-minor :chords ["C5 A4 E4 A3" "B4 G#4 E4 E3" "C5 A4 E4 A3"] :expect #{}}
   :leading-tone {:key c-major :chords ["G4 D4 B3 G3" "G4 E4 G3 C3"] :expect #{:leading-tone :doubling}}})
