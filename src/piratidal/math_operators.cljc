(ns piratidal.math-operators
  (:require
   [clojure.core]
   [piratidal.parser :refer [maybe-parse-pattern]]))

(defn string-or-numbers? [xs]
  (every? #(or (string? %) (number? %)) xs))

(defn pattern-constructor? [x]
  (:pattern-constructor (meta x)))

(defn pattern? [x]
  (:pattern/type x))

(defn op-dispatcher
  [& xs]
  (let [[x1 x2 & xs-rest2] xs]
    (cond
      (every? number? xs) :num-op
      (string-or-numbers? xs) :str-pattern-op
      (and (pattern-constructor? x1) (string-or-numbers? (rest xs))) :str-pattern-op-with-constructor
      (and (pattern? x1) (pattern-constructor? x2) (string-or-numbers? (rest xs-rest2))) :str-pattern-op-with-pattern-and-constructor)))

(defn safe-op
  [op]
  (fn [& args]
    (try
      (apply (resolve (symbol "clojure.core" (str op))) args)
      #?(:clj (catch  java.lang.ArithmeticException e
                (println (.getMessage e) "defaulting to 1")
                1)
         :cljs (catch  js/Error e
                 (println (.getMessage e) "defaulting to 1")
                 1)))))

(defmacro def-pattern-ops
  [& ops]
  (mapv (fn [op]
          `(do (defmulti ~op
                 #'op-dispatcher)

               (defmethod ~op :num-op
                 [~'& ~'nums]
                 (apply (#'safe-op ~(str op)) ~'nums))

               (defmethod ~op :str-pattern-op
                 [~'x ~'& ~'xs]
                 {:pattern/type :math-op
                  :op (#'safe-op ~(str op))
                  :value (maybe-parse-pattern ~'x)
                  :op-patterns (map maybe-parse-pattern ~'xs)})

               (defmethod ~op :str-pattern-op-with-constructor
                 [~'constructor ~'x ~'& ~'xs]
                 {:pattern/type :math-op
                  :op (#'safe-op ~(str op))
                  :pattern-constructor ~'constructor
                  :value (maybe-parse-pattern ~'x {:value-type (:type (meta ~'constructor))})
                  :op-patterns (map #(maybe-parse-pattern % {:value-type (:type (meta ~'constructor))}) ~'xs)})

               (defmethod ~op :str-pattern-op-with-pattern-and-constructor
                 [~'pattern ~'constructor ~'& ~'xs]
                 {:pattern/type :math-op
                  :op (#'safe-op ~(str op))
                  :pattern-constructor ~'constructor
                  :value ~'pattern
                  :op-patterns (map #(maybe-parse-pattern % {:value-type (:type (meta ~'constructor))}) ~'xs)
                  :apply-structure :left})))
        ops))

(comment
  (mod piratidal.api/n "7.5 2 3" "11 13")
  (map :value
       (piratidal.pattern/query (mod "7.5 2 3" "11 13")
                                [0 2])))
