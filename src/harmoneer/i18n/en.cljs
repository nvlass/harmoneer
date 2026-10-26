(ns harmoneer.i18n.en
  (:require [clojure.string :as str]
            [harmoneer.theory :as t]
            [harmoneer.harmony :as h]))

;; ---------------------------------------------------------------- formatting

(defn- note [loc n] (t/note-name n (:names loc)))
(defn- pc [loc n] (t/pc-name n (:names loc)))

(defn- key-name [loc {:keys [tonic mode]}]
  (str (t/tonic-name tonic (:names loc)) (if (= mode :major) " major" " minor")))

(def ^:private voice-names {:s "soprano" :a "alto" :t "tenor" :b "bass"})

(defn- voice [v] (voice-names v))
(defn- Voice [v] (str/capitalize (voice-names v)))

(def ^:private roles {0 "root" 1 "third" 2 "fifth"})

(def ^:private iv-words
  {1 "unison" 2 "2nd" 3 "3rd" 4 "4th" 5 "5th" 6 "6th" 7 "7th" 8 "octave"})

(def ^:private q-words
  {:P "perfect" :M "major" :m "minor" :A "augmented" :d "diminished"
   :AA "doubly augmented" :dd "doubly diminished"})

(defn- iv-long [{:keys [num q]}]
  (if (> num 8)
    (str "more than an octave")
    (str (q-words q) " " (iv-words num))))

(defn- a [s] (if (re-find #"^[aeiou]" s) (str "an " s) (str "a " s)))

(defn- chord [key c] (str (h/numeral key c) (h/inversion-figure (:inv c))))

(defn- perfects [p] (if (zero? p) "octaves" "fifths"))

;; ---------------------------------------------------------------- dictionary

(def dict
  {:fmt
   {:note note
    :pc pc
    :key key-name
    :chord (fn [_ key c] (chord key c))
    :voice (fn [_ v] (Voice v))
    :voice-abbr (fn [_ v] ({:s "S" :a "A" :t "T" :b "B"} v))
    :signature (fn [_ s] (cond (zero? s) "no sharps or flats" (pos? s) (str s " ♯") :else (str (- s) " ♭")))}

   :nav {:write "Exercise" :rules "Rules"}

   :level
   {:1 "Primary degrees" :2 "Inversions" :3 "VI and II" :4 "Cadences" :5 "III and VII"
    :desc
    {:1 "I, IV and V in root position: doubling, connecting chords, common tones."
     :2 "I, IV and V in first inversion, and 6/4 chords as passing or pedal chords."
     :3 "VI and II: V–VI, two common tones, II and II6 before the dominant."
     :4 "Perfect cadence with the cadential 6/4, plagal and deceptive cadences."
     :5 "III, and VII in first inversion."}}

   :v
   {:chord (fn [loc key c notes]
             (str "Not in " (chord key c) ": " (str/join ", " (map #(pc loc %) notes)) "."))
    :no-chord (fn [_ _] "These four notes don't make a chord of the key.")
    :vocab (fn [_ key c level] (str (chord key c) " isn't part of level " level " yet."))
    :missing (fn [_ r] (str "The " (roles r) " is missing."))
    :fifth "The fifth is missing; leave it out only in the final chord."
    :double-lt "The leading tone is doubled."
    :doubling (fn [_ key c r]
                (let [ok (h/good-doublings key c nil)]
                  (str "Doubled " (roles r) " in " (chord key c) "; double the "
                       (str/join " or the " (map roles (sort ok))) " instead.")))
    :crossing (fn [_ u l] (str "The " (voice u) " is below the " (voice l) "."))
    :spacing (fn [_ u l] (str "More than an octave between " (voice u) " and " (voice l) "."))
    :range (fn [_ v high?] (str "The " (voice v) " is too " (if high? "high" "low") "."))
    :parallel (fn [_ u l p] (str "Parallel " (perfects p) " between " (voice u) " and " (voice l) "."))
    :antiparallel (fn [_ u l p] (str (str/capitalize (perfects p)) " by contrary motion between " (voice u) " and " (voice l) "."))
    :hidden (fn [_ p] (str "Hidden " (if (zero? p) "octave" "fifth") ": the outer voices reach it in similar motion."))
    :overlap (fn [_ u l] (str "The " (voice u) " and " (voice l) " overlap: one passes the other's previous note."))
    :common-tone "The common tone isn't kept in the same voice."
    :contrary "No common tone: the upper voices should move against the bass."
    :leading-tone (fn [_ v] (str "The leading tone in the " (voice v) " doesn't rise to the tonic."))
    :melodic (fn [_ v iv] (str "The " (voice v) " moves by " (a (iv-long iv)) "."))
    :inner-leap (fn [_ v iv] (str "The " (voice v) " leaps " (a (iv-long iv)) "; inner voices keep common tones or move by step."))
    :six-four (fn [_ kind]
                (if (= kind :cadential)
                  "The cadential 6/4 belongs on a strong beat."
                  "A 6/4 chord only as cadential, passing (bass by step) or pedal (bass held) 6/4."))
    :progression (fn [_ key x y] (str (chord key x) " → " (chord key y) " goes backwards, from dominant to subdominant."))
    :harmonic-rhythm "The same chord repeats from a weak beat to the next downbeat."}

   :rule
   {:chord "Chord tones"
    :vocab "Chords of the level"
    :complete "Root and third"
    :fifth "Complete chords"
    :double-lt "Leading tone not doubled"
    :doubling "Doubling"
    :doubling-third "No doubled major third"
    :crossing "Voice crossing"
    :spacing "Spacing"
    :range "Voice ranges"
    :parallel "Parallel fifths and octaves"
    :antiparallel "Contrary fifths and octaves"
    :hidden "Hidden fifths and octaves"
    :hidden-leap "Hidden fifths and octaves by leap"
    :overlap "Overlapping voices"
    :common-tone "Common tones"
    :contrary "Contrary motion to the bass"
    :leading-tone "Leading tone resolution"
    :leading-tone-outer "Leading tone in the outer voices"
    :melodic "Melodic intervals"
    :inner-leap "Inner voice leaps"
    :six-four "6/4 chords"
    :progression "Retrogressions"
    :harmonic-rhythm "Harmonic rhythm"
    :desc
    {:chord "Every note belongs to the chord: the one the figures ask for, or one of the key."
     :vocab "With a given melody, use the chords of the chosen level."
     :complete "Every chord has its root and third."
     :fifth "Every chord has its fifth, except the final one, where three roots and a third are fine."
     :double-lt "The leading tone is never doubled."
     :doubling "Root position: double the root (the third in VI after V, in II° and VII°). First inversion: not the third of I, IV or V. 6/4: double the bass."
     :doubling-third "Don't double the third of a major chord."
     :crossing "Soprano, alto, tenor and bass stay in order."
     :spacing "At most an octave between soprano and alto, and between alto and tenor."
     :range "Soprano C4–G5, alto G3–D5, tenor C3–G4, bass E2–C4."
     :parallel "No two voices move in parallel fifths, octaves or unisons."
     :antiparallel "No fifths or octaves by contrary motion either."
     :hidden "The outer voices don't reach a fifth or octave in similar motion."
     :hidden-leap "The outer voices reach a fifth or octave in similar motion only with the soprano moving by step."
     :overlap "No voice moves past the note the voice next to it just had."
     :common-tone "Between root-position chords, a common tone stays in the same voice and the others move to the nearest notes."
     :contrary "Between root-position chords with no common tone (IV–V, V–VI), the upper voices move against the bass."
     :leading-tone "The leading tone rises to the tonic when V or VII goes to I or VI."
     :leading-tone-outer "In the soprano and bass the leading tone rises to the tonic; an inner voice may fall to the fifth."
     :melodic "No augmented intervals, sevenths or leaps beyond an octave in any voice."
     :inner-leap "Alto and tenor leap no more than a fourth."
     :six-four "A 6/4 is cadential (I6/4 → V on a strong beat), passing (bass by step in one direction) or pedal (bass held, same chord around it)."
     :progression "With a given melody: V and VII don't go to IV or II."
     :harmonic-rhythm "With a given melody: don't repeat a chord from a weak beat to the next downbeat."}}

   :group {:chord "Chords" :motion "Connecting chords" :melody "Voice leading" :progression "Progression"}
   :preset {:strict "Strict" :relaxed "Relaxed" :custom "Custom"}

   :ui
   {:loading-sounds "loading sounds…"
    :language "Language"
    :level "Level"
    :type "Given"
    :type-bass "Figured bass"
    :type-soprano "Melody"
    :key "Key"
    :random-major "Any major key"
    :random-minor "Any minor key"
    :random-any "Any key"
    :bars "Length"
    :bars-n (fn [_ n] (str n " bars"))
    :new "New exercise"
    :generating "Writing an exercise…"
    :no-exercise "Choose a level and press New exercise."
    :task-bass "Write the soprano, alto and tenor over the figured bass."
    :task-soprano "Harmonize the melody: write the alto, tenor and bass."
    :sel-info (fn [loc c v n] [:<> [:b (Voice v)] " · chord " (inc c) (when n (str " · " (note loc n)))])
    :click-staff "Click the staff, or type note letters."
    :tool-up "Up a step (↑)" :tool-down "Down a step (↓)"
    :tool-sharp "Sharp (#)" :tool-flat "Flat (-)" :tool-natural "Natural (n)"
    :tool-delete "Delete (⌫)" :tool-undo "Undo (Ctrl+Z)" :tool-redo "Redo"
    :fill "Hint"
    :fill-title "Fill the selected chord with a voicing that fits"
    :play "Play" :stop "Stop"
    :solution "Solution" :hide-solution "Hide solution"
    :solution-title "One solution"
    :solution-hint "There are usually several good ones; this is the one the exercise was made from."
    :clear "Clear"
    :rule-check "Check"
    :progress (fn [_ k n] (str k " of " n " chords written"))
    :complete "All chords written."
    :no-violations "No rule broken."
    :done "Clean. Well done!"
    :penalty "penalty"
    :results-hint (fn [_ e w] (str "Errors count " e ", warnings " w ". Click a line to highlight it."))
    :cols (fn [_ cs] (if (= 1 (count cs)) (str "chord " (inc (first cs))) (str "chords " (inc (first cs)) "–" (inc (last cs)))))
    :mute "mute" :mute-title "Turn this rule off"
    :try "Try: "
    :keys-hint "Keys: C–B write a note in the selected voice and move to the next one · ↑↓ step (Shift: octave) · # - n accidentals · ←→ chords · Tab voice · ⌫ delete · Space play. With a MIDI keyboard, play the whole chord at once."
    :rule-set "Rule set"
    :rule-set-hint "Strict keeps the textbook rules of the first year. Relaxed allows what most teachers accept: moving the common tone, hidden fifths with the soprano by step, an inner leading tone falling to the fifth."
    :settings "Settings"
    :note-names "Note names"
    :hear "Hear while writing"
    :hear-chord "the chord" :hear-note "the note" :hear-off "nothing"
    :analysis "Roman numerals under complete chords"
    :midi-toggle "MIDI keyboard"
    :midi-connected (fn [_ ins] (str "Connected: " (str/join ", " ins)))
    :midi-none "No MIDI device found."
    :midi-unsupported "This browser doesn't support Web MIDI."
    :midi-denied "MIDI access was denied."
    :midi-off "Play chords on a MIDI keyboard."}})
