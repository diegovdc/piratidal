(ns piratidal.scratch
  (:require
   [clojure.core :omit [+ -]]
   [clojure.string :as str]
   [piratidal.parser :refer [parse-pattern]]
   [piratidal.pattern :refer [query]]))

(+ "1" "2")
(- api/note 1 2 3)
(- (with-meta {} {:pattern-constructor true}) 1 2 3)

(macroexpand-1
 '(def-pattern-ops [+ -]))
(+ (with-meta {} {:pattern-constructor true}) "1 2 3" "1 2 3")

(+ {:value true} (with-meta {} {:pattern-constructor true}) "1 2 3" "1 2 3")

#_(defmethod + :pattern
    [& [pat & pats]]
    (apply map
           (fn [& xs] (apply clojure.core/+ (map :value xs)))
           pat
           (map cycle pats)))

(+ [{:value 1} {:value 3} {:value 6}]
   [{:value 2}]
   [{:value 1} {:value -1}])




;; Order readme items :
;;


(map (fn [s])
     (str/split))
"- [x] cat
- [x] fastcat
- [] timeCat
- [] randcat
- [] wrandcat
- [] append
- [] fastAppend
- [] wedge
- [] brak
- [] listToPat
- [] fromList
- [] fromMaybes
- [] flatpat
- [] run
- [] scan
- [] overlay
- [] <>
- [x] stack
- [] superimpose
- [x] layer
- [] steps
- [] iter
- [] iter'
- [] range
- [] rangex
- [] quantise
- [x] degrade
- [x] degradeBy
- [x] unDegradeBy
- [] Repetitions
- [] ply
- [] stutter
- [] stripe
- [] slowstripe
- [x] palindrome
- [] trunc
- [] linger
- [] chunk
- [] chunk'
- [] loopFirst
- [] bite
- [] shuffle
- [] scramble
- [x] rot
- [] step
- [] step'
- [] lindenmayer
- [] spread
- [] spreadf
- [] fastspread
- [] spreadChoose
- [] resetCycles
- [] setcps
- [] trigger
- [] qtrigger
- [] qt
- [] mtrigger
- [] mt
- [] triggerWith
- [] all
- [] once
- [] every
- [] every'
- [] foldEvery
- [] when
- [] whenT
- [] whenmod
- [] ifp
- [] fix
- [] unfix
- [] contrast
- [] contrastBy
- [] choose
- [] chooseby
- [] wchoose
- [] wchooseby
- [] select
- [] selectF
- [] pickF
- [] squeeze
- [] inhabit
- [] struct
- [] mask
- [] sew
- [] stitch
- [s] euclid
- [] euclidInv
- [] euclidFull
- [] fix
- [] fixRange
- [] ifp
- [x] fast
- [x] fastGap
- [x] slow
- [x] sparsity
- [] hurry
- [] slowSqueeze
- [] fastSqueeze
- [] compress
- [] zoom
- [] within
- [] stretch
- [] off
- [] press
- [] pressBy
- [x] rotL
- [x] rotR
- [] spin
- [] weave
- [] weaveWith
- [x] rev
- [x] jux
- [] juxBy
- [] Swing
- [] swingBy
- [] swing
- [] ghost
- [] ghost'
- [] ghostWith
- [] Inside and outside
- [] inside
- [] outside
- [] echo
- [] echoWith
- [] stut
- [] stutWith
- [] scale
- [] scaleList
- [] scaleTable
- [] getScale
- [] toScale
- [] Chords
- [] chordList
- [] chordTable
- [] arpeggiate
- [] arp
- [] rolled
- [] rolledBy
- [] Chord Modifiers/Voicings
- [] Number of Chord tones
- [] Open voicing
- [] Drop N voicings
- [] Chord Inversions
- [] anticipate
- [] anticipateIn
- [] clutch
- [] clutchIn
- [] histpan
- [] interpolate
- [] interpolateIn
- [] jump
- [] jumpIn
- [] jumpIn'
- [] jumpMod
- [] wait
- [] waitT
- [] wash
- [] washIn
- [] xfade
- [] xfadeIn
- [x] amp
- [x] begin
- [x] end
- [x] gain
- [] grain
- [] grain'
- [] accelerate
- [x] speed
- [x] sustain
- [] unit
- [] timescale
- [] timescalewin
- [] chop
- [] striate
- [] striateBy
- [] slice
- [] splice
- [] randslice
- [] chew
- [] loopAt
- [] smash
- [] smash'
- [] segment
- [] discretise
- [] sig
- [] rand
- [] irand
- [] perlin
- [] perlinWith
- [] perlin2
- [] perlin2With
- [x] sometimes
- [x] sometimesBy
- [x] someCycles
- [x] someCyclesBy
- [] choose
- [] chooseby
- [] wchoose
- [] wchooseby
- [] cycleChoose
- [] ur
- [] seqP
- [] seqPLoop"





;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;


(map (juxt :value :arc/active)
     (query
      {:pattern/type :fastcat
       :len 2
       :value
       [{:pattern/type :atom, :value "bd"}
        {:pattern/type :degrade-by
         :value {:pattern/type :atom, :value "bd"}
         :probability 0.5}]}
      [0 10]))

