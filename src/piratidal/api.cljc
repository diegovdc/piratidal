(ns piratidal.api
  "This namespace is only for generating the API of piratidal.
  The implemented patterns and pattern transformation functions
  are declared at the begining of the file. The actual api functions
  are generated with macros.

  For the implemetation of the pattern transformation functions see the `piratidal.pattern` namespace."
  (:refer-clojure :exclude [loop + * / - mod quot])
  (:require
   [piratidal.math-operators :refer [* +]]
   [piratidal.parser :refer [maybe-parse-pattern parse-pattern
                             with-param-pattern]]
   [piratidal.pattern :refer [query]]
   [piratidal.utils :refer [deep-assoc-value-type]]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns and effects
;;;;;;;;;;;;;;;;;;;;;;;;

(declare s sound n note gain speed
         ;; TODO what are lag, unit, loop , delta and offset... are they public from the user's perspective?
         begin end length accelerate unit loop delta legato sustain amp channel pan
         freq midinote octave lag offset cut orbit shape hcutoff hresonance bandf
         bandq crush coarse cutoff attack release hold tremolorate tremolodepth
         phaserrate phaserdepth tilt plat vowel delaytime delayfeedback delayAmp
         delaySend lock size room dry leslie lrate lsize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns transformation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The function arguments are declared here as well. TODO tell cljkondo to autodeclare all these things for these macros
(declare almost-always almost-never amount degrade degrade-by euclidean
         f fast fastGap fs jux layer off often palindrome probability
         pulses rarely rev rotation rotl rotr slow somecycles-by
         sometimes somtimes-by speed steps superimpose undegrade-by)

(defn main-pattern
  [value-type pat-str]
  (if (:pattern/type pat-str)
    (deep-assoc-value-type pat-str value-type)
    (parse-pattern pat-str {:value-type value-type})))

(defn ctl-pattern
  [value-type ctl-str pat]
  (if (:pattern/type ctl-str)
    {:pattern/type :control-pattern
     :value pat
     :controls [(deep-assoc-value-type ctl-str value-type)]}
    (let [ctl (parse-pattern ctl-str {:value-type value-type})]
      {:pattern/type :control-pattern
       :value pat
       :controls [ctl]})))

(defn pattern-fn [pattern-type params pat]
  (let [param-patterns (map (fn [[k v]] (maybe-parse-pattern v {:value-type k}))
                            ;;  do not include functions, which are passed as the `:f` or `:fs` param
                            (dissoc params :f :fs))]

    (cond-> {:pattern/type pattern-type
             :value (maybe-parse-pattern pat)}
      (seq param-patterns) (#(with-param-pattern param-patterns %))
      (:f params) (assoc :f (:f params))
      (:fs params) (assoc :fs (:fs params)))))

(defmacro def-main-and-control-patterns
  [syms]
  (mapv (fn [sym]
          `(let [pattern-maker#
                 (fn ~sym
                   ([~'pat-str] (#'main-pattern ~(keyword sym) ~'pat-str))
                   ([~'pat ~'ctl-str] (#'ctl-pattern ~(keyword sym) ~'ctl-str ~'pat)))]
             (def ~sym (with-meta pattern-maker# {:pattern-constructor true :type ~(keyword sym)}))))
        syms))

(defmacro def-pattern-transformations
  [syms-and-args]
  (mapv (fn [[sym args]]
          `(defn ~sym
             ~(apply conj ['pat] args)
             (pattern-fn ~(keyword sym)
                         ~(into {}
                                (map (fn [arg] {(keyword arg) arg})
                                     args))
                         ~'pat)))
        syms-and-args))

(comment
  (macroexpand-1
   '(def-main-and-control-patterns
      [sound n note gain speed]))

  (macroexpand-1
   '(def-pattern-transformations
      [[slow [speed]] [fast [speed]]])))

;;;;;;;;;;;
;; Generate the patterns and pattern transformation functions


#_(def-main-and-control-patterns
    [s n note gain speed sound
     begin end length accelerate unit loop delta legato sustain amp channel pan
     freq midinote octave lag offset cut orbit shape hcutoff hresonance bandf
     bandq crush coarse cutoff attack release hold tremolorate tremolodepth
     phaserrate phaserdepth tilt plat vowel delaytime delayfeedback delayAmp
     delaySend lock size room dry leslie lrate lsize])

#_(def-pattern-transformations
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

(def sound s)

(comment
  (-> (sound "bd sn cp")
      (gain "1 0.5")
      (* n "1 2 3" "2")
      palindrome
      (slow 2)
      (sometimes (fn [_] (-> (note "1 2") (gain "3"))))
      (query [0 2])))

(comment
  ;; TODO convert to tests
  ;; possible operator notations

  (-> (sound "bd sn cp")
      (n "1 2 3 4")
      (+ n "2")
      #_(* n "2")
      (query [0 1]))

  (-> (sound "bd sn cp")
      (n "1 2 3")
      ;; TODO make this work, and syntax may be ugly here
      (+ n (-> "2 5" (slow 2)) "2")
      #_(query [0 1]))

  (+ n "1 2 3" "1 2")

  (query (n (sound "bd sn cp") (+ "1 2 3" "1 2 5"))
         [0 1])

  (query (n (sound "bd sn cp") (+ "1 2 3" "1 2 5"))
         [0 1])

  (query (+ "1 2 3" "1 2 5")
         [0 1]))
