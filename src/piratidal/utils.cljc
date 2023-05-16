(ns tidal-mini.utils)

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (concat (drop off a) (take off a))))

(defn wrap-at [i coll]
  (let [size (count coll)
        i* (if (zero? size) 0 (mod i size))]
    (nth coll i* nil)))
