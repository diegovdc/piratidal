(ns tidal-mini.query2
  (:require [tidal-mini.parser :refer [parse-pattern]]))

(defn pat->query2
    ;; NOTE `cycles` is a list of int
  [pattern cycles]
  (into [] (mapcat (fn [cycle]
                     (->> pattern
                          (query {:index 0 :elapsed-arc 0 :cycle cycle})))
                   cycles)))
[(pat->query2 (parse-pattern "<hh cp>*3 bd") [1])
 "================="
 #_(-> [{:fast {:word "bd"}, :speed 2}]
       (pat->query2 [0]))
 "================="
 #_(-> [{:fast {:word "bd"}, :speed 2/3}]
       (pat->query2 [1]))]
