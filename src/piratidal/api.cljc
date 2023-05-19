(ns piratidal.api
  (:require
   [clojure.core]
   [piratidal.math-operators :refer [* +]]
   [piratidal.parser :refer [maybe-parse-pattern parse-pattern
                             with-param-pattern]]
   [piratidal.pattern :refer [query]]
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

(defn pattern-fn [pattern-type params pat]
  (let [param-patterns (map (fn [[k v]] (maybe-parse-pattern v {:value-type k}))
                            ;;  do not include functions, which are passed as the `:f` param
                            (dissoc params :f))]

    (cond-> {:pattern/type pattern-type
             :value (maybe-parse-pattern pat)}
      (seq param-patterns) (#(with-param-pattern param-patterns %))
      (:f params) (assoc :f (:f params)))))

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
      [sound n note gain speed])))
(declare sound n note gain speed)



;; Patterns


(def-main-and-control-patterns
  [sound n note gain speed sound])
(comment
  (meta n))
;; Aliases

(def s sound)

(macroexpand-1
 '(def-pattern-transformations
    [[slow [speed]]
     [fast [speed]]
     [rotl [amount]]
     [rotr [amount]]
     [rev []]
     [palindrome []]
     [somecycles-by [probability f]]]))

;; Patterns transformation functions
;; The function arguments are declared here as well. TODO tell cljkondo to autodeclare all these things for these macros
(declare amount f fast palindrome probability rev rotl rotr slow somecycles-by sometimes speed)
(def-pattern-transformations
  [[slow [speed]]
   [fast [speed]]
   [rotl [amount]]
   [rotr [amount]]
   [rev []]
   [palindrome []]
   [somecycles-by [probability f]]
   [sometimes [f]]])

(-> (sound "bd sn cp")
    (gain "1 0.5")
    (* n "1 2 3" "2")
    #_palindrome
    (slow 2)
    #_(sometimes (fn [_] (-> (note "1 2") (gain "3"))))
    (query [0 2]))

(comment
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
