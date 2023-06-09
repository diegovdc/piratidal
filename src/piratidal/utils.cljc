(ns piratidal.utils
  (:require
   [clojure.walk :as walk]))

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off a) (take off a))))

(defn wrap-at [i coll]
  (let [size (count coll)
        i* (if (zero? size) 0 (mod i size))]
    (nth coll i* nil)))

(defn wrap-nth
  [coll index]
  (nth coll (mod index (count coll))))

(defn deep-assoc-value-type
  [ctl-pattern value-type & {:keys [validate-value-fn]}]
  (walk/postwalk
   (fn [x] (if (= (:pattern/type x) :atom)
             (do (when validate-value-fn
                   (validate-value-fn x))
                 (assoc x :value/type value-type))
             x))
   ctl-pattern))

(defn cps->bpm [cps]
  (* 60 cps))
