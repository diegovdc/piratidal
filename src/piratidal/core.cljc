(ns piratidal.core
  (:refer-clojure :exclude [loop + - mod quot * /])
  (:require
   [piratidal.api
    :refer [def-main-and-control-patterns def-pattern-transformations]]
   [piratidal.math-operators :refer [def-pattern-ops]]
   [piratidal.pattern]
   [piratidal.playback :as playback :refer [start-ticker]]
   [time-time.dynacan.players.gen-poly :as gp]
   [potemkin :refer [import-vars]]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns and effects
;;;;;;;;;;;;;;;;;;;;;;;;
#_:clj-kondo/ignore
(def-main-and-control-patterns ;; TODO what are lag, unit, loop , delta and offset... are they public from the user's perspective?
  s n note gain speed sound
  begin end length accelerate unit loop delta legato sustain amp channel pan
  freq midinote octave lag offset cut orbit shape hcutoff hresonance bandf
  bandq crush coarse cutoff attack release hold tremolorate tremolodepth
  phaserrate phaserdepth tilt plat vowel delaytime delayfeedback delayAmp
  delaySend lock size room dry leslie lrate lsize scale2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The function arguments are declared here as well. TODO tell cljkondo to autodeclare all these things for these macros
(declare almost-always almost-never amount degrade degrade-by euclidean
         f fast fastGap fs jux layer off often palindrome probability
         pulses rarely rev rotation rotl rotr slow somecycles-by
         sometimes somtimes-by speed steps superimpose undegrade-by)
#_:clj-kondo/ignore
(def-pattern-transformations
  [[slow [speed]]
   [fast [speed]]
   [fastGap [speed]]
   [rotl [amount]]
   [rotr [amount]]
   [rev []]
   [palindrome []]
   [somecycles-by [probability f]]
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

(import-vars
 [piratidal.pattern sometimes-by sometimes often rarely almost-always almost-never])

#_:clj-kondo/ignore
(def-pattern-ops * + - / mod quot)

;; Aliases
#_:clj-kondo/ignore
(def sound s)

(defn init!
  "Start tick and OSC communication"
  ([] (init! 1))
  ([cps]
   (println "Initializing piratidal!")
   (start-ticker cps)))

(defn p
  [id pattern]
  (when-not (playback/playing?)
    (init!))
  (prn-str
    ;; HACK to realize lazy stuff in the sequence
   (swap! playback/patterns assoc id pattern))
  [:pattern id])

(defn hush []
  (gp/stop)
  (reset! playback/patterns {})
  :hushing...)

(defn setcps
  ([cps]
   (start-ticker cps)))

(comment
  ;; FIXME something is not quite right when changing cps
  ;; Depends on the moment that the cps changes, probably it could
  ;; be a good idea to set the cycle change until the next cycle starts
  (setcps 1)
  (setcps 2))
