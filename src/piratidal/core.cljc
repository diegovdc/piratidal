(ns piratidal.core
  (:require
   [piratidal.api
    :refer [def-main-and-control-patterns def-pattern-transformations]]
   [piratidal.playback :as playback :refer [start-ticker ticker-state]]
   [time-time.dynacan.players.gen-poly :as gp]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns and effects
;;;;;;;;;;;;;;;;;;;;;;;;

(declare s sound n note gain speed
         ;; TODO what are lag, unit, loop , delta and offset... are they public from the user's perspective?
         begin end length accelerate unit loop delta legato sustain amp channel pan
         freq midinote octave lag offset cut orbit shape hcutoff hresonance bandf
         bandq crush coarse cutoff attack release hold tremolorate tremolodepth
         phaserrate phaserdepth tilt plat vowel delaytime delayfeedback delayAmp
         delaySend lock size room dry leslie lrate lsize crush)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The function arguments are declared here as well. TODO tell cljkondo to autodeclare all these things for these macros
(declare almost-always almost-never amount degrade degrade-by euclidean
         f fast fastGap fs jux layer off often palindrome probability
         pulses rarely rev rotation rotl rotr slow somecycles-by
         sometimes somtimes-by speed steps superimpose undegrade-by)

(def-main-and-control-patterns
  [s n note gain speed sound
   begin end length accelerate unit loop delta legato sustain amp channel pan
   freq midinote octave lag offset cut orbit shape hcutoff hresonance bandf
   bandq crush coarse cutoff attack release hold tremolorate tremolodepth
   phaserrate phaserdepth tilt plat vowel delaytime delayfeedback delayAmp
   delaySend lock size room dry leslie lrate lsize])

(def-pattern-transformations
  [[slow [speed]]
   [fast [speed]]
   [fastGap [speed]]
   [rotl [amount]]
   [rotr [amount]]
   [rev []]
   [palindrome []]
   [somecycles-by [probability f]]
   [somtimes-by [probability f]]
   [sometimes [f]]
   [almost-always [f]]
   [often [f]]
   [rarely [f]]
   [almost-never [f]]
   [superimpose [f]]
   [off [amount f]]
   [degrade []]
   [degrade-by [probability]]
   [undegrade-by [probability]]
   [jux [f]]
   [layer [fs]]
   [euclidean [pulses steps rotation]]
   ;; TODO can stack, slowcat (and other cats)  be done here?
   ])

;; Aliases
#_:clj-kondo/ignore
(def sound s)


;; Patterns


(defn init!
  "Start tick and OSC communication"
  ([] (start-ticker 1))
  ;; TODO
  ([cps] (start-ticker cps)))

(defmacro p
  [id pattern & patterns]
  `(let [pattern# (-> ~pattern ~@patterns)]
     (when-not (playback/playing?)
       (init!))
     (swap! playback/patterns assoc ~id pattern#)
     pattern#))

(defn setcps
  ;; FIXME
  ([cps]
   #_(swap! ticker-state assoc :cps 2)
   #_(start-ticker cps)))

(defn hush []
  (gp/stop))

(comment
  (hush)
  (init! 1)
  (p 1 (sound "[bd cp/2 hh]*2")
     (gain "{1 0.8 0.7}%4")
     #_(fast 3)
     #_(rarely #(-> % (fast 3) (delaytime 0.5)))
     (crush "[2 3 4, 16]")
     palindrome
     #_(degrade-by 0.8)
     #_(jux #(-> % #_(degrade-by 0.2) (fast 2)))))
