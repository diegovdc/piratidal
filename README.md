# Piratidal

Experimental Clojure port of [TidalCycles](https://github.com/tidalcycles/). This is a very early work in progress and so far more of a proof of concept.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Piratidal](#piratidal)
  - [Installation](#installation)
  - [Usage](#usage)
  - [(un)Implemented Features](#unimplemented-features)
    - [Parser](#parser)
    - [Tidal's API Implemented So Far](#tidals-api-implemented-so-far)

<!-- markdown-toc end -->

## Installation

Piratidal requires the installation of Java, the `clojure-cli` and a suitable editor (one with a Clojure repl). You will also need to download `SuperCollider` and install `SuperDirt`.

- Java - If you don't already have Java installed I recommend [open-jdk](https://openjdk.org/).

- `clojure-cli` - https://clojure.org/guides/install_clojure

- Editor - If you are new to Clojure a good place to start might be the [Calva](https://calva.io/getting-started/) VSCode plugin. However you can find many other options in the following link https://clojure.org/guides/editors or https://practical.li/clojure/clojure-editors/#clojure-aware-editors. If you just want to try `Piratidal` without an editor repl, you can simply start a repl with the `clojure-cli`.
  - If using the `clojure-cli`, navigate to the piratidal directory in a terminal, and call the `clojure` or the `clj` command.
- SuperCollider - SuperCollider can be downloaded from here: https://supercollider.github.io/downloads. `SuperDirt` can be installed by following these instructions: [Linux](https://tidalcycles.org/docs/getting-started/linux_install/#superdirt-installation), [MacOs](https://tidalcycles.org/docs/getting-started/macos_install#superdirt), [Windows](https://tidalcycles.org/docs/getting-started/windows_install#superdirt)

## Usage

Open SuperCollider and evaulate the following line of code:

```supercollider
SuperDirt.start
```

The start a Clojure repl by following the instructions of the plugin from your editor of choice or running the following line of code in your terminal (while inside the piratidal directory).

```bash
$ clj
```

Then in an editor file or your terminal repl evaluate the following:

```clj
(require '[piratidal.core :refer :all])

(p 1 (-> (s "[bd cp/2 hh]")
         (jux rev)))
```

You can also use the examples file at `dev/examples.cljc` to try other stuff out.

The API that has been already implemented can be found in the `piratidal.core` namespace in the `def-main-and-control-patterns`, `def-pattern-transformations` and `import-vars` forms. If using the `->` (thread) macro, the API replicates very closely Tidal's own.

## (un)Implemented Features

### Parser

Most of the parser's functionality is already implemented. Notable missing things are the following:

0. Elongation probably doesn't work 100%.
1. Patterned operations such as `bd*<1 2>`. Operations missing this are fast, slow, euclidean rhythms, elongation, replication and degradation.
2. The fastcat operator `.`.
3. Variables
4. Chords

### Tidal's API Implemented So Far

- [x] amp
- [x] begin
- [x] cat - not yet exposed by the API
- [x] degrade
- [x] degradeBy
- [x] euclid
- [x] end
- [x] fast
- [x] fastGap
- [x] fastcat
- [x] gain
- [x] hush
- [x] jux
- [x] layer - not yet exposed by the API
- [x] palindrome
- [x] rev
- [x] rot
- [x] rotL
- [x] rotR
- [x] slow
- [x] someCycles
- [x] someCyclesBy
- [x] sometimes
- [x] sometimesBy
- [x] sparsity
- [x] speed
- [x] stack - not yet exposed by the API
- [x] sustain
- [x] unDegradeBy
- [ ] accelerate
- [ ] all
- [ ] anticipate
- [ ] anticipateIn
- [ ] append
- [ ] arp
- [ ] arpeggiate
- [ ] bite
- [ ] brak
- [ ] chew
- [ ] choose
- [ ] choose
- [ ] chooseby
- [ ] chooseby
- [ ] chop
- [ ] chordList
- [ ] chordTable
- [ ] chunk
- [ ] chunk'
- [ ] clutch
- [ ] clutchIn
- [ ] compress
- [ ] contrast
- [ ] contrastBy
- [ ] cycleChoose
- [ ] discretise
- [ ] echo
- [ ] echoWith
- [ ] euclidFull
- [ ] euclidInv
- [ ] every
- [ ] every'
- [ ] fastAppend
- [ ] fastSqueeze
- [ ] fastspread
- [ ] fix
- [ ] fix
- [ ] fixRange
- [ ] flatpat
- [ ] foldEvery
- [ ] fromList
- [ ] ~fromMaybes~
- [ ] getScale
- [ ] ghost
- [ ] ghost'
- [ ] ghostWith
- [ ] grain
- [ ] grain'
- [ ] histpan
- [ ] hurry
- [ ] ifp
- [ ] ifp
- [ ] inhabit
- [ ] inside
- [ ] interpolate
- [ ] interpolateIn
- [ ] irand
- [ ] iter
- [ ] iter'
- [ ] jump
- [ ] jumpIn
- [ ] jumpIn'
- [ ] jumpMod
- [ ] juxBy
- [ ] lindenmayer
- [ ] linger
- [ ] listToPat
- [ ] loopAt
- [ ] loopFirst
- [ ] mask
- [ ] mt
- [ ] mtrigger
- [ ] off
- [ ] once
- [ ] outside
- [ ] overlay
- [ ] perlin
- [ ] perlin2
- [ ] perlin2With
- [ ] perlinWith
- [ ] pickF
- [ ] ply
- [ ] press
- [ ] pressBy
- [ ] qt
- [ ] qtrigger
- [ ] quantise
- [ ] rand
- [ ] randcat
- [ ] randslice
- [ ] range
- [ ] rangex
- [ ] resetCycles
- [ ] rolled
- [ ] rolledBy
- [ ] run
- [ ] scale
- [ ] scaleList
- [ ] scaleTable
- [ ] scan
- [ ] scramble
- [ ] segment
- [ ] select
- [ ] selectF
- [ ] seqP
- [ ] seqPLoop
- [ ] setcps
- [ ] sew
- [ ] shuffle
- [ ] sig
- [ ] slice
- [ ] slowSqueeze
- [ ] slowstripe
- [ ] smash
- [ ] smash'
- [ ] spin
- [ ] splice
- [ ] spread
- [ ] spreadChoose
- [ ] spreadf
- [ ] squeeze
- [ ] step
- [ ] step'
- [ ] steps
- [ ] stitch
- [ ] stretch
- [ ] striate
- [ ] striateBy
- [ ] stripe
- [ ] struct
- [ ] stut
- [ ] stutWith
- [ ] stutter
- [ ] superimpose
- [ ] swing
- [ ] swingBy
- [ ] timeCat
- [ ] timescale
- [ ] timescalewin
- [ ] toScale
- [ ] trigger
- [ ] triggerWith
- [ ] trunc
- [ ] unfix
- [ ] unit
- [ ] ur
- [ ] wait
- [ ] waitT
- [ ] wash
- [ ] washIn
- [ ] wchoose
- [ ] wchoose
- [ ] wchooseby
- [ ] wchooseby
- [ ] weave
- [ ] weaveWith
- [ ] wedge
- [ ] when
- [ ] whenT
- [ ] whenmod
- [ ] within
- [ ] wrandcat
- [ ] xfade
- [ ] xfadeIn
- [ ] zoom
