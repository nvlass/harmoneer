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

   :nav {:write "Exercise" :cheat "Cheat sheet" :rules "Rules"}

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
    :hidden (fn [_ p] (str "Hidden " (if (zero? p) "octave" "fifth") ": the outer voices reach it in similar motion, the soprano by leap."))
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
    :hidden-octave "Hidden octaves"
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
     :hidden "The outer voices reach a fifth or octave in similar motion only with the soprano moving by step (as in V–I with 2→1 in the soprano)."
     :hidden-octave "The outer voices reach an octave in similar motion only with the soprano moving by step; hidden fifths are fine."
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

   :fix
   {:chord "Use only the notes of the chord; check the accidentals the figures ask for (♯ under the bass means a raised third)."
    :vocab "Look for a chord of the level that contains the melody note."
    :complete "Move a doubled note to the missing root or third."
    :fifth "Replace one of the doubled notes with the fifth."
    :double-lt "Keep the leading tone in one voice; double the root instead."
    :doubling "Change the doubled note to the root (or, in a first inversion, to the fifth)."
    :doubling-third "Double the root or the fifth instead of the third."
    :crossing "Swap the two notes, or move one of them by an octave."
    :spacing "Bring the upper voices closer: move the lower one up or the upper one down by an octave."
    :range "Move the note by an octave, or pick another chord tone."
    :parallel "Move one of the two voices the other way, or give it another chord tone."
    :antiparallel "Keep the interval from repeating: change one voice to another chord tone."
    :hidden "Let the soprano move by step, or move the bass in contrary motion."
    :hidden-octave "Let the soprano move by step, or move the bass in contrary motion."
    :overlap "Move the voice to a note on its own side of its neighbour's previous note."
    :common-tone "Keep the shared note in the same voice and move the others to the nearest notes."
    :contrary "Move soprano, alto and tenor down when the bass goes up (and the other way round)."
    :leading-tone "Move the leading tone up a half step to the tonic."
    :leading-tone-outer "Move the leading tone in the soprano up a half step to the tonic."
    :melodic "Choose another chord tone nearer the previous note; in minor, avoid the step 6→7 (augmented 2nd)."
    :inner-leap "Keep the common tone or move the inner voice to the nearest chord tone."
    :six-four "Use the 6/4 on a downbeat before V (cadential), between two positions of one chord with the bass stepping (passing), or over a held bass (pedal)."
    :progression "Go from IV or II to V, not the other way round."
    :harmonic-rhythm "Change the chord on the downbeat, or repeat it within the bar instead."}

   :cheat
   {:voices-title "Four voices"
    :voices [:<>
             [:p "Soprano and alto share the treble staff (stems up, down), tenor and bass the bass staff. Ranges: soprano C4–G5, alto G3–D5, tenor C3–G4, bass E2–C4."]
             [:p "Between soprano and alto, and between alto and tenor, at most an octave; the bass may be further from the tenor. In " [:b "close position"] " the three upper voices fit in an octave; in " [:b "open position"] " there is room for another chord tone between them."]]
    :ex-close "I in close position."
    :ex-open "I in open position."
    :degrees-title "The chords of the key"
    :degrees [:p "A triad on each degree, with the notes of the scale. In minor the chords come from the " [:b "harmonic"] " minor scale, with the raised seventh: V is major, VII diminished, III augmented. The roman numeral names the degree of the root."]
    :th-major "C major" :th-minor "A minor" :th-function "Function"
    :q-major "major" :q-minor "minor" :q-dim "diminished" :q-aug "augmented" :q-other ""
    :fn-t "tonic" :fn-s "subdominant" :fn-d "dominant"
    :doubling-title "Doubling"
    :doubling [:<>
               [:p "Four voices, three notes: one note is doubled."]
               [:ul
                [:li [:b "Root position"] ": double the root. Exceptions: VI after V doubles the third, and so do II° and VII°."]
                [:li [:b "First inversion"] ": in I6, IV6 and V6 double the root or fifth, not the bass; in II6 the bass (third) is the best doubling."]
                [:li [:b "Second inversion"] " (6/4): double the bass."]
                [:li "Never double the " [:b "leading tone"] "."]]]
    :ex-double-root "Root doubled in every chord: I IV V I."
    :ex-double-third "The third of a major chord doubled."
    :connect-title "Connecting chords"
    :connect [:<>
              [:p [:b "Common tone"] ": when two chords share a note, keep it in the same voice and move the other upper voices to the nearest notes (I–IV, I–V, I–VI)."]
              [:p [:b "No common tone"] ": when the roots are a step apart (IV–V, V–VI) the upper voices move against the bass, to avoid parallels."]
              [:p "V–VI is the exception: the leading tone rises to the tonic with the bass, the other two voices fall, and VI ends up with a doubled third."]]
    :ex-common-tone "I–IV–I: C stays in the tenor."
    :ex-no-common "IV–V–I: the bass rises, the upper voices fall."
    :ex-deceptive "V–VI: the leading tone rises, VI has a doubled third."
    :forbidden-title "Forbidden motion"
    :forbidden [:<>
                [:p [:b "Parallel fifths and octaves"] ": two voices a fifth (or an octave, or a unison) apart both move to the next fifth (octave) in the same direction. They are errors between any two voices."]
                [:p [:b "Antiparallels"] ": the same, by contrary motion (a fifth to a twelfth, an octave to a unison)."]
                [:p [:b "Hidden fifths and octaves"] ": the outer voices reach a fifth or octave in similar motion. Fine with the soprano moving by step (V–I with 2→1), faulty with a leap."]
                [:p "Voices also shouldn't " [:b "cross"] " (alto above soprano) or " [:b "overlap"] " (a voice moving past the note its neighbour just had)."]]
    :ex-parallels "All voices rising with the bass: parallel fifths and octaves."
    :ex-hidden "A hidden octave: the soprano leaps to G with the bass."
    :leading-title "The leading tone"
    :leading [:<>
              [:p "The seventh degree, a half step below the tonic. When V (or VII) goes to I or VI it rises to the tonic. In minor it needs an accidental (G♯ in A minor), written in every chord that has it, and in figures as ♯ under the bass."]
              [:p "Relaxed rules let an inner leading tone fall to the fifth of I, to complete the chord; strict rules don't."]]
    :ex-minor "A minor: I–V–I with G♯."
    :ex-leading-tone "The leading tone falls in the tenor, and I ends up with a doubled fifth."
    :inversions-title "Inversions and figures"
    :inversions [:<>
                 [:p "The bass decides the position: root in the bass is " [:b "root position"] " (5/3, usually written as nothing), the third in the bass is the " [:b "first inversion"] " (6), the fifth is the " [:b "second inversion"] " (6/4). The figures are intervals above the bass."]
                 [:p "An accidental in the figures changes that note: ♯ alone means a raised third (V in minor), ♯6 a raised sixth."]]
    :ex-inversions "I, I6, I6/4. The 6/4 on its own is unstable: it needs one of the uses below."
    :six-four-title "The 6/4 chord"
    :six-four [:ul
               [:li [:b "Cadential"] ": I6/4 on a downbeat before V, over the same bass (the στηριγμένη τέλεια πτώση). The sixth and fourth fall to the fifth and third."]
               [:li [:b "Passing"] ": between two positions of one chord, the bass passing by step (I – V6/4 – I6)."]
               [:li [:b "Pedal"] ": over a held bass, between the same chord (I – IV6/4 – I)."]]
    :ex-cadential "I–IV–I6/4–V–I: the cadential 6/4 on the downbeat."
    :ex-passing "I–V6/4–I6: the bass passes C–D–E."
    :ex-pedal "I–IV6/4–I over a held C."
    :cadences-title "Cadences"
    :cadences [:ul
               [:li [:b "Perfect"] " (τέλεια): V–I, both in root position, the tonic in the soprano for the strongest ending. With the cadential 6/4 it is the στηριγμένη τέλεια πτώση."]
               [:li [:b "Plagal"] " (πλάγια, ecclesiastical): IV–I."]
               [:li [:b "Half"] " (ημίπτωση): ending on V."]
               [:li [:b "Deceptive"] " (απατηλή): V–VI instead of V–I."]]
    :ex-perfect "IV–V–I."
    :ex-plagal "IV–I."
    :ex-half "I–V."
    :melody-title "Harmonizing a melody"
    :melody [:ol.steps
             [:li "Start from the cadence: the last two chords (V–I, or I6/4–V–I) and the bass under them."]
             [:li "For each melody note, list the chords of the level that contain it."]
             [:li "Choose so the harmony moves forward (I → IV or II → V → I), changes on downbeats, and doesn't go back from V to IV or II."]
             [:li "Write the bass: mostly contrary motion to the melody, a few inversions to make it move by step."]
             [:li "Fill in alto and tenor with common tones and steps, checking doubling and parallels."]]
    :rules-title "Rules checked"
    :rules-hint [:p.hint "Errors are the rules every textbook agrees on; warnings are style points. Click a violation in the exercise to see its fix."]
    :level-error "error" :level-warning "warning"
    :strict-only "strict" :relaxed-only "relaxed" :melody-only "melody exercises"
    :about-title "About the score"
    :about (fn [_ e w]
             [:<>
              [:p "Each broken rule adds a penalty (" e " for an error, " w " for a warning). Zero means nothing the rules know about is wrong, not that it is the only or the most beautiful answer: compare with the solution, and listen to both."]])}

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
    :rule-set-hint "Strict keeps the textbook rules of the first year. Relaxed allows what most teachers accept: moving the common tone, hidden fifths, an inner leading tone falling to the fifth."
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
