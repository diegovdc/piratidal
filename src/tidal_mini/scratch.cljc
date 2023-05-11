(ns tidal-mini.scratch
  (:require [clojure.core]))

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
