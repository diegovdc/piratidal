(ns tidal-mini.polymeter
  (:require [tidal-mini.utils :refer [wrap-at]]))

(defn polymeter-step-at-cycle&index
  "Calculate the number of times a current step has been seen in the polymeter. For debugging purposes will return a map wit the value-index and the times-seen

  The following tables are examples of the calculation
  For example in a {1 2}%3 polymeter:
  0     1     2       ; cycle
  0 1 2 0 1 2 0 1 2   ; current-step-index
  1 2 1 2 1 2 1 2 1   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; value-index
  0 0 1 1 2 2 3 3 4   ; times-seen

  And a {1 2 3}%4 polymeter:
  0       1       2           ; cycle
  0 1 2 3 0 1 2 3 0 1 2  3    ; current-step-index
  1 2 3 1 2 3 1 2 3 1 2  3    ; current-polymeter-step
  0 1 2 3 4 5 6 7 8 9 10 11   ; value-index
  0 0 0 1 1 1 2 2 2 3 3  3    ; times-seen

  And a {1 2 3}%2 polymeter:
  0   1   2   3   4   ; cycle
  0 1 0 1 0 1 0 1 0   ; current-step-index
  1 2 3 1 2 3 1 2 3   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; value-index
  0 0 0 1 1 1 2 2 2   ; times-seen"
  [polymeter-size polymeter-steps cycle current-step-index]
  (let [value-index (+ (* cycle polymeter-steps) current-step-index)]
    {:value-index value-index
     :times-seen (quot value-index polymeter-size)}))

(do

  (defn polymeter->stack
    [cycle {:keys [polymeter steps]}]
    {:stack
     (let [cats (-> polymeter :stack)]
       (map (fn [cat]
              (:cat
               (reduce
                (fn [acc step]
                  (let [pat (wrap-at (+ (* cycle steps) step) cat)
                        slowcat? (:slowcat pat)
                        pat* (if-not slowcat? pat
                                     (assoc pat :cycle (:times-seen
                                                        (polymeter-step-at-cycle&index
                                                         (count cat)
                                                         steps
                                                         cycle
                                                         step))))]
                    (update acc :cat conj pat*)))
                {:cat []
                 :slowcat-indexes {}}
                (range steps))))
            cats))})
  (polymeter->stack
   1
   {:polymeter
    {:stack
     [[{:word "bd"} {:slowcat [{:stack [[{:word "hh"} {:word "cp"} 808]]}]}]]}
    :steps 3}))
