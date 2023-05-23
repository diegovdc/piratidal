(ns user
  (:require [overtone.at-at :refer [now]]))

(defmacro dbg-time
  [msg f]
  `(let [n# (now)
         res# ~f]
     (println ~msg (- (now) n#))
     res#))

