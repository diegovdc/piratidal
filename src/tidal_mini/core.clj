(ns tidal-mini.core
  (:require
   [tidal-mini.control-patterns :refer [apply-ctl-pattern gain]]
   [tidal-mini.parser :refer [parse-pattern]]
   [tidal-mini.utils :refer [wrap-at]]))

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

(do
  (defn pattern-length
    [pattern]
    (reduce (fn [acc el]
              (if (:elongated el)
                (+ acc (:size el))
                (inc acc)))
            0
            pattern))
  (pattern-length [{:elongated {:word "bd"}, :size 2} {:word "bd"}]))

(do

  (defn take-slow-segment
    ;; FIXME use `elapsed-arc` and `end-arc`
    ;; TODO  refactor for performance and clarity
    ;; TODO also the logic for this and for the `:fast` case should be the same but the speed value would be inverted
    [cycle ratio speed events]
    (->> events
         (map (fn [event-data]
                (update event-data :arc
                        (fn [arc]
                          (map #(* % speed) arc)))))
         (filter (fn [{:keys [arc]}]
                   (let [min* (* (mod cycle speed) ratio)
                         max* (+ min* ratio)]
                     (and (<= min* (first arc))
                          (< (first arc) max*)))))
         (map (fn [event-data]
                (update event-data :arc
                        (fn [arc]
                          (mapv #(- % (* (mod cycle speed) ratio))
                                arc)))))))

  (take-slow-segment
   3 1/2 3
   [{:event {:word "a"}, :arc [0 1/4]}
    {:event {:word "b"}, :arc [1/4 1/2]}]))


;; TODO WIP, arc transposition and slow segment  think it's working, needs cleanup
;;


(do
  (defn extend-arc
    [speed event]
    (update event :arc (partial map #(* speed %))))

  (extend-arc
   3
   #_{:event {:word "a"}, :arc [0 1/4]}
   {:event {:word "a"}, :arc [1/2 3/4]}))

(do

  (defn translate-arc
    [arc-span reference-arc event-arc]
    (let [[start end] reference-arc]
      (loop [[ev-start ev-end] event-arc]
        (cond
          (and (<= start ev-start)
               (< ev-start end)) [ev-start (min end ev-end)]
          (<= end ev-start) (recur [(- ev-start arc-span)
                                    (- ev-end arc-span)])
          (< ev-start start) (recur [(+ ev-start arc-span)
                                     (+ ev-end arc-span)])))))
  (translate-arc 1/2 [0 1/2] [3/4 1]))

(do

  (defn take-slow-segment2
    [{:keys [speed cycle elapsed-arc end-arc]}
     events]
    (let [ratio (- end-arc elapsed-arc)
          expanded-events (map (partial extend-arc speed) events)
          spans (partition 2 1 (range (* speed elapsed-arc) (+ ratio (* speed end-arc)) ratio))
          [start end] (wrap-at cycle spans)]
      (->> expanded-events
           (reduce
            (fn [acc {:keys [arc] :as event}]
              (let [[ev-start] arc]
                (if-not (and (<= start ev-start)
                             (< ev-start end))
                  acc
                  (conj acc (assoc event :arc
                                   (translate-arc ratio [elapsed-arc end-arc] arc))))))
            []))))
  (take-slow-segment2
   {:speed 1
    :cycle 1
    :elapsed-arc 1/2
    :end-arc 1}
   [{:event {:word "a"}
     :arc [1/2 3/4]}
    {:event {:word "b"}
     :arc [3/4 1N]}]))

(do
  ;; #dbg
  (defn make-schedule*
    [{:keys [index elapsed-arc ratio cycle slow-cat? polymeter-steps]
      :or {index 0
           elapsed-arc 0
           ratio 1
           cycle 0
           slow-cat? false}}
     pattern]
    (let [length (case pattern
                   :stack (pattern-length (:stack pattern))
                   :alt 1
                   :polymeter 1
                   (pattern-length pattern))]
      (:events (reduce
                (fn [{:as acc :keys [index elapsed-arc]} x]
                  (let [end-arc (+ elapsed-arc (* (/ 1 length) ratio
                                        ; if `:elongated`
                                                  (:size x 1)))
                        event (cond
                                (or  (:word x)
                                     (int? x)
                                     (double? x)
                                     (ratio? x)
                                     (= x :silence)) {:event x
                                                      :arc [elapsed-arc end-arc]}

                                (:elongated x)
                                (make-schedule*
                                 {:index 0
                                  :elapsed-arc elapsed-arc
                                  :ratio (* (/ ratio length)
                                            (:size x))
                                  :cycle cycle}
                                 [(:elongated x)])

                                (vector? x) (make-schedule*
                                             {:index index
                                              :elapsed-arc elapsed-arc
                                              :ratio (/ ratio length)
                                              :cycle cycle
                                              :slow-cat? slow-cat?}
                                             x)

                                (:fast x)
                                (map
                                 #(make-schedule*
                                   {:index 0
                                    :elapsed-arc (+ elapsed-arc
                                                    (* % (/ ratio length (:speed x))))
                                    :ratio (/ ratio length (:speed x))
                                    :cycle (+ (* (:speed x) cycle) %)}
                                   [(:fast x)])
                                 (range (:speed x)))

                                (:slow x)
                                (let [ratio* (/ ratio length)]
                                  (->> (make-schedule*
                                        {:index 0
                                         :elapsed-arc elapsed-arc
                                         :ratio ratio*
                                         :cycle (quot cycle (:speed x))}
                                        [(:slow x)])
                                       flatten
                                       #_(take-slow-segment cycle ratio* (:speed x))
                                       (take-slow-segment2
                                        {:speed (:speed x)
                                         :cycle cycle
                                         :elapsed-arc elapsed-arc
                                         :end-arc end-arc})
                                       #_(filter (fn [{:keys [arc] :as ev}]
                                                   (let [range* (/ 1 (* (:speed x) ratio*))
                                                         min* (* (mod cycle (:speed x)) range*)
                                                         max* (+ min* range*)]
                                                     (and (<= min* (first arc))
                                                          (< (first arc) max*)))))
                                       #_(map (fn [event-data]
                                                (update event-data :arc
                                                        (fn [arc]
                                                          (map #(* % (:speed x))
                                                               arc)))))))

                                (and slow-cat? (:stack x))
                                (->> x
                                     :stack
                                     (map (fn [pat]
                                            (let [cycle* (if polymeter-steps
                                                           (mod cycle polymeter-steps)
                                                           cycle)]
                                              [(wrap-at cycle* pat)])))
                                     (map #(make-schedule*
                                            {:index 0
                                             :elapsed-arc elapsed-arc
                                             :ratio (/ ratio length)
                                             :cycle cycle}
                                            %)))

                                (:stack x)
                                (map
                                 #(make-schedule*
                                   {:index 0
                                    :elapsed-arc elapsed-arc
                                    :ratio (/ ratio length)
                                    :cycle cycle}
                                   %)
                                 (:stack x))

                                (:alt x)
                                (make-schedule*
                                 {:index index
                                  :elapsed-arc elapsed-arc
                                  :ratio (/ ratio length)
                                  :cycle (:cycle x cycle)
                                  :slow-cat? true}
                                 (:alt x))

                                (:polymeter x)
                                (make-schedule*
                                 {:index index
                                  :elapsed-arc elapsed-arc
                                  :ratio (/ ratio length)
                                  :cycle cycle}
                                 [(polymeter->stack cycle x)])

                                (:ctl-type x)
                                (apply-ctl-pattern
                                 (partial make-schedule*
                                          {:index index
                                           :elapsed-arc elapsed-arc
                                           :ratio (/ ratio length)
                                           :cycle cycle})
                                 x)

                                :else (println "Warning, unknown pattern: " x))]
                    (if-not event
                      acc
                      (-> acc
                          (update :index inc)
                          (update :events conj event)
                          (assoc :elapsed-arc end-arc
                                 :slow-cat? false)))))
                {:index index :elapsed-arc elapsed-arc :events []}
                pattern))))

  (defn make-schedule
    [config pattern]
    (flatten (make-schedule* config pattern)))
  #_(->> "<bd sn>"
         parse-tidal
         transform-tree
         (make-schedule {:index 0 :elapsed-arc 0})
         flatten)
  (-> "a b c"
      parse-pattern
      (gain "1 <2 1> 3")
      (->> (make-schedule {:index 0 :elapsed-arc 0 :cycle 0}))))
