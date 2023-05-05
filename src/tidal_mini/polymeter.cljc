(ns tidal-mini.polymeter
  (:require [tidal-mini.utils :refer [wrap-at]]))

(defn polymeter-step-at-cycle&index
  "Calculate the number of times a current step has been seen in the polymeter. For debugging purposes will return a map wit the event-index and the times-seen

  The following tables are examples of the calculation
  For example in a {1 2}%3 polymeter:
  0     1     2       ; cycle
  0 1 2 0 1 2 0 1 2   ; current-step-index
  1 2 1 2 1 2 1 2 1   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; event-index
  0 0 1 1 2 2 3 3 4   ; times-seen

  And a {1 2 3}%4 polymeter:
  0       1       2           ; cycle
  0 1 2 3 0 1 2 3 0 1 2  3    ; current-step-index
  1 2 3 1 2 3 1 2 3 1 2  3    ; current-polymeter-step
  0 1 2 3 4 5 6 7 8 9 10 11   ; event-index
  0 0 0 1 1 1 2 2 2 3 3  3    ; times-seen

  And a {1 2 3}%2 polymeter:
  0   1   2   3   4   ; cycle
  0 1 0 1 0 1 0 1 0   ; current-step-index
  1 2 3 1 2 3 1 2 3   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; event-index
  0 0 0 1 1 1 2 2 2   ; times-seen"
  [polymeter-size polymeter-steps cycle current-step-index]
  (let [event-index (+ (* cycle polymeter-steps) current-step-index)]
    {:event-index event-index
     :times-seen (quot event-index polymeter-size)}))

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
                        alt? (:alt pat)
                        pat* (if-not alt? pat
                                     (assoc pat :cycle (:times-seen
                                                        (polymeter-step-at-cycle&index
                                                         (count cat)
                                                         steps
                                                         cycle
                                                         step))))]
                    (update acc :cat conj pat*)))
                {:cat []
                 :alt-indexes {}}
                (range steps))))
            cats))})
  (polymeter->stack
   1
   {:polymeter
    {:stack
     [[{:word "bd"} {:alt [{:stack [[{:word "hh"} {:word "cp"} 808]]}]}]]}
    :steps 3}))
