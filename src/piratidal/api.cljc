(ns piratidal.api
  "This namespace is only for generating the API of piratidal.
  The implemented patterns and pattern transformation functions
  are declared at the begining of the file. The actual api functions
  are generated with macros.

  For the implemetation of the pattern transformation functions see the `piratidal.pattern` namespace."
  (:refer-clojure :exclude [loop + * / - mod quot])
  (:require

   [piratidal.parser :refer [maybe-parse-pattern parse-pattern
                             with-param-pattern]]
   [piratidal.pattern :refer [sometimes-by query]]
   [piratidal.pattern :refer [apply-off-fn]]
   [piratidal.utils :refer [deep-assoc-value-type]]))

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

(defn apply-f
  [pattern-type params value]
  (case pattern-type
    :off (apply-off-fn (:f params) (:amount params) value)
    :sometimes-by (sometimes-by (:f params) (:probability params) value)
    ((:f params) value)))

(defn pattern-params?
  [pattern-type param-patterns]
  (cond
    (#{:off :sometimes-by} pattern-type) false
    :else (seq param-patterns)))

(defn pattern-fn [pattern-type params pat]
  (let [param-patterns (map (fn [[k v]] (maybe-parse-pattern v {:value-type k}))
                            ;;  do not include functions, which are passed as the `:f` or `:fs` param
                            (dissoc params :f :fs))
        value (maybe-parse-pattern pat)]

    (cond-> {:pattern/type pattern-type
             :value value}
      (:f params) (assoc :fvalue (apply-f pattern-type params value))
      (:fs params) (assoc :fs (:fs params))
      (pattern-params? pattern-type param-patterns) (#(with-param-pattern param-patterns %)))))

(defmacro def-main-and-control-patterns
  [& syms]
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

#_(def sound s)

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
