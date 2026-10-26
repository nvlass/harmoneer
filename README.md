# Harmoneer

Four-part harmony exercises in the browser, following the first year of harmony in Μπάμπης Κανάς's *Αρμονία*: primary degrees, inversions, VI and II, cadences, III and VII. No backend; it installs as a PWA and works offline.

Sibling of [Chord Trainer](https://github.com/nvlass/chordtrainer), [Counterpointer](https://github.com/nvlass/counterpointer) and [Dicteeineer](https://github.com/nvlass/dicteeineer), at [music-trainer.app](https://music-trainer.app).

## Exercises

- **Figured bass**: write soprano, alto and tenor over a given bass with figures.
- **Melody**: harmonize a given soprano with alto, tenor and bass. The app names the chords you write.

Exercises are generated: a progression from a table of usual successions for the chosen level, a bass line, and a voicing found by the solver, which checks it against the same rules the student is checked with. "Solution" shows that voicing; "Hint" voices the selected chord to fit what is around it.

Every rule only reports, never blocks. Strict and Relaxed presets (as in Counterpointer) decide which rules count; each one can be muted.

Input: click the staff, type note letters (the cursor moves down the voices, then to the next chord), arrows and `#` `-` `n` to adjust, or play whole chords on a MIDI keyboard.

A **Cheat sheet** covers the first-year theory (voices, chords of the key, doubling, connecting chords, forbidden motion, the leading tone, inversions and figures, 6/4 chords, cadences, harmonizing a melody) with examples checked by the same rules, and lists every rule with its fix.

English and Greek.

## Development

```sh
npm install
npx shadow-cljs watch app      # http://localhost:8283
npx shadow-cljs compile test   # tests on node
npm run dist                   # release build into dist/
```

Piano samples: FluidR3_GM (CC BY 3.0).
