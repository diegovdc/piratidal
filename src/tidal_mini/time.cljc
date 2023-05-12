(ns tidal-mini.time
  (:require [tidal-mini.parser :refer [parse-pattern]]))

(defn inc-cycle
  [n]
  (int (inc n)))

(defn inc-arc
  [n arc]
  (map #(+ n %) arc))

(do
  (defn span-cycles
    [[start end]]
    (loop [b (int start)
           spans []]
      (let [e (inc-cycle b)]
        (cond
          (< end b) []
          (= b end) spans
          (> e end) (conj spans [b end])
          :else (recur e (conj spans [b e]))))))

  #_(span-cycles [1/2 2]))
(defmulti query
  (fn [data] (:pattern/type data)))

(defmethod query :atom
  [{:keys [value query-arc]}]
  (map (fn [active]
         (let [whole-b (int (first active))
               whole-e (inc whole-b)]
           {:value value
            ;; :arc/whole [whole-b whole-e]
            :arc/active active}))
       (span-cycles query-arc)))

(declare fast slow)

(defmethod query :fast
  [pattern-data]
  (fast pattern-data))

(defmethod query :slow
  [pattern-data]
  (slow pattern-data))

(do
  (defn slowcat
    [{:keys [value query-arc]}]
    (->> value
         (map-indexed
          (fn [i pat]
            (mapcat (fn [arc]
                      (query (assoc pat :query-arc (inc-arc i arc))))
                    (span-cycles query-arc))))
         flatten))

  (defmethod query :slowcat
    [data]
    (slowcat data))

  (defmethod query :stack
    [{:keys [query-arc] :as data}]
    (let [[start] query-arc]
      (mapcat
       (fn [i arc]
         (let [events (slowcat (assoc data :query-arc (inc-arc i arc)))
               active-arcs (map :arc/active events)
               min-active (apply min (map first active-arcs))
               max-active (apply max (map second active-arcs))
               ratio (- max-active min-active)]
           (reduce
            (fn [acc {:keys [arc/active] :as ev}]
              (let [updated-arc (map #(/ % ratio) active)
                    updated-ev (assoc ev :arc/active updated-arc)]
                (cond
                  (and (< (first updated-arc) start)
                       (<= (second updated-arc) start))
                  acc

                  (and (< (first updated-arc) start)
                       (> (second updated-arc) start))
                  (conj acc (assoc updated-ev :partial? true))

                  :else (conj acc updated-ev))))
            []
            events)))
       (range)
       (span-cycles query-arc))))

  #_(defn fastcat
      [{:keys [value query-arc] :as data}]
      (mapcat (fn [v]
                (println v)
                (fast {:speed (count value)
                       :value (assoc v :pattern/type :atom)
                       :query-arc query-arc}))
              (slowcat data)))

  #_(defmethod query :fastcat
      [data]
      (fastcat data))

  (query {:pattern/type :stack
          :value [{:pattern/type :slow
                   :value {:pattern/type :atom :value "bd"}
                   :speed 2}
                  {:pattern/type :atom :value "cp"}]
          :query-arc [0 2]})
  #_(query {:pattern/type :fastcat
            :value [{:pattern/type :atom :value "bd"}
                    {:pattern/type :atom :value "cp"}]
            :query-arc [0 1]})
  #_(query {:pattern/type :fast
            :value {:pattern/type :atom :value "bd"}
            :speed 2}))

;then can use this like this:


(query {:pattern/type :atom
        :value        5
        :query-arc    [1/2 2]})

(do
  (defn update-span-time
    [timef arc]
    (map timef arc))

  (defn update-event-time
    [{:as event :keys [arc/active]} timef]
    (assoc event :arc/active (map timef active)))

  (defn with-query-time
    [pat timef arc]
    (query (assoc pat :query-arc (update-span-time timef arc))))

  (defn fast [{:keys [speed value query-arc]}]
    (-> value
        (with-query-time #(* % speed) query-arc)
        (->> (map (fn [ev] (update-event-time ev #(/ % speed)))))))

  (defn slow [pattern-data]
    (fast (update pattern-data :speed #(/ 1 %))))

  (fast {:speed 3
         :value {:pattern/type :atom :value "bd"}
         :query-arc [0 1]})
  (slow {:speed 3
         :value {:pattern/type :atom :value "bd"}
         :query-arc [0 9]}))

(query {:pattern/type :slow
        :speed 2
        :value {:pattern/type :atom
                :value "bd"}
        :query-arc [1 3]})

(query {:pattern/type :fast
        :speed 3
        :value {:pattern/type :atom :value "bd"}
        :query-arc [0 9]})

{:pattern/type :cat
 :value [{:word "bd"} {:word "cp"}]}

(do
  (defn starts-in-cycle?
    [cycle [arc-start]]
    (<= cycle arc-start))
  (starts-in-cycle? 1 [0 2]))

(do
  (defn ends-in-cycle?
    [cycle [_ arc-end]]
    (and (< cycle arc-end)
         (<= arc-end (inc cycle))))
  (ends-in-cycle? 0 [0 2]))

(defn arc-lenght
  [[start end]]
  (- end start))

(do
  (defn rev-event
    ;; TODO figure out if arc/active or arc/whole should be used
    [current-cycle event]
    (let [ends-in-cycle?* (ends-in-cycle? current-cycle
                                          (:arc/active event))
          reverse-arc (fn [arc]
                        (let [[start end] arc
                              next-cycle (int (inc current-cycle))
                              start* (+ current-cycle (- next-cycle end))
                              end* (- next-cycle start)]
                          [(if ends-in-cycle?* start*
                               (max current-cycle start*))
                           end*]))]
      (cond-> event
        :always (update :arc/active reverse-arc)
        (not ends-in-cycle?*) (assoc :value :silence
                                     :original-value (:value event)))))

  (rev-event 0 {:value "bd" :arc/active [0 1/2]})
  ;; => {:value "bd", :arc/active [1/2 1]}

  (rev-event 0 {:value "bd" :arc/active [0 1]})
  ;; => {:value "bd", :arc/active [0 1]}

  (rev-event 0 {:value "bd" :arc/active [0 2]})
  ;; => {:value :silence, :arc/active [0 1], :original-value "bd"}
  ;; => {:value :silence, :arc/active [0 1], :original-value "bd"}

  (rev-event 1 {:value "bd" :arc/active [0 2]})
  ;; => {:value "bd", :arc/active [1 2]}

  (rev-event 1 {:value "bd" :arc/active [0 3/2]})
  ;; => {:value "bd", :arc/active [1 2]}
  )

(do
  (defn odd-cycle? [cycle-arc]
    (odd? (first cycle-arc)))

  (defn palindrome-cycles
    "Slow down the query-arc to repeat each cycle twice so that the odd one can be reversed"
    [query-arc]
    (map (fn [cycle-arc]
           (let [start (int (/ (first cycle-arc) 2))
                 arc [start (inc start)]]
             arc))
         (span-cycles query-arc)))
  (palindrome-cycles [1/2 4]))

