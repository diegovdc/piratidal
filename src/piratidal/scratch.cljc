(ns piratidal.scratch
  (:require [clojure.core]
            [clojure.string :as str]
            [piratidal.pattern :refer [query]]))

(defn +dispatcher
  [& xs]
  (cond
    (every? number? xs) :numbers
    (and (seq (first xs))
         (:value (first (first xs)))) :pattern))

(defmulti +
  #'+dispatcher)

(defmethod + :numbers
  [& xs]
  (apply clojure.core/+ xs))

(defmethod + :pattern
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


(query {:pattern/type :fastcat
        :len 2
        :value
        [{:pattern/type :atom, :value "bd"}
         {:pattern/type :stack
          :value
          [{:pattern/type :fastcat
            :len 3
            :value
            [{:pattern/type :atom, :value "bd"}
             {:pattern/type :atom, :value "sn"}
             {:pattern/type :atom, :value "hh"}]}]}]}
       [0 1])
