# Harmoneer

Static re-frame/shadow-cljs SPA for four-part harmony (SATB) exercises, no backend. Sibling of `../counterpointer` (rule presets, violations list, i18n pattern) and `../dicteeineer` (`theory`, `audio`, `midi`, `storage`, samples were copied from it). See README.

## Commands

- `npx shadow-cljs watch app`: dev server on :8283
- `npx shadow-cljs compile test`: runs `test/` on node (slow-ish: it generates an exercise for every key × level × type)
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose (deploy by copying it). Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- Notes are `{:s step :a alt}` (C4 = 28); keys `{:tonic {:l :a} :mode}`. Chords use harmonic minor (`t/scale-alt`), so in minor V is major, VII° diminished, III+ augmented. A chord is `{:d degree :inv 0|1|2}`; roles are 0 root, 1 third, 2 fifth (`h/role`).
- An exercise is `{:key :level :type :bass|:soprano :given :b|:s :chords :solution :cols}`. `:cols` is the student's answer, one `{:s :a :t :b}` map per chord, with the given voice filled in and never editable. Chords are half notes, two per 4/4 bar, and the last is a whole note on a downbeat: 2 × bars − 1 columns, even columns are strong beats.
- `theory`, `harmony`, `rules`, `solver`, `exercise` are pure. Rules are metadata in `rules/rules` plus a `check-col`, `check-pair` or `check-all` method; a violation is `{:cols :voices :msg [:v/<key> & args]}`. For a figured bass the chord is the given one (`:chords`); for a melody it's what the four notes spell (`h/identify`), only once the column is complete.
- `solver/solve` is dynamic programming over columns using the strict rules' col and pair checks as costs (error 1000, warning 25) plus voice-leading smoothness. The generator and the Hint button both use it; a new col/pair rule automatically shapes the solutions, so check `generated-solutions-are-clean` after adding one.
- Levels (`h/levels`) add chords following Κανάς year 1. Year 2 (sevenths, V9, modulation) is the next step; see TODO.
- Persistence: IndexedDB keys `settings`, `exercise` (EDN strings), saved by the global `persist` interceptor. New setting keys need a default in `events/default-settings`.
- Translations: tongue, `i18n/en.cljs` and `i18n/el.cljs` with the same keys (`i18n-test` checks this and renders every violation message). In Greek, keep voice names in the nominative (parentheses or after a colon) and never use `;` as a separator.
- Hiccup: never return a vector of vectors as children, and don't pass a map with a `:key` entry as a component's first argument (Reagent takes it as the React key): score props call the key signature `:keysig`.
- `resources/public/sw.js` lists the app shell and samples; bump `VERSION` when the cached set changes.
