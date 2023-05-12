(ns tidal-mini.query2)

(defn inc-cycle
  [n]
  (int (inc n)))

(defn inc-arc
  [n arc]
  (map #(+ n %) arc))

(defn map-arc
  [event arc-kw f]
  (update event arc-kw #(map f %)))

(defn map-arcs
  [f arc-kw events]
  (map #(map-arc % arc-kw f) events))

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

(defmulti query
  (fn [data _query-arc] (:pattern/type data :event)))

(defn sam [n] (int n))

(defn next-sam [n] (inc (sam n)))

(defmethod query :atom
  [{:keys [value]} query-arc]
  (map (fn [active]
         {:value value
          :arc/whole [(sam (first active)) (next-sam (first active))]
          :arc/active active})
       (span-cycles query-arc)))

(defmethod query :event
  ;; NOTE This might not be the best thing to do as an event is technically not a pattern
  [data _query-arc]
  data)

(defn update-span-time
  [timef arc]
  (map timef arc))

(defn update-event-time
  [{:as event :keys [arc/active arc/whole]} timef]
  (assoc event
         :arc/whole (mapv timef whole)
         :arc/active (mapv timef active)))

(defn with-query-time
  [pat timef arc]
  (query pat (update-span-time timef arc)))

(defn fast [{:keys [speed value]} query-arc]
  (-> value
      (with-query-time #(* % speed) query-arc)
      (->> (map (fn [ev]
                  (let [ev* (update-event-time ev #(/ % speed))]
                    (if (<= (first query-arc) (first (:arc/active ev*)))
                      ev*
                      (assoc ev* :has-start? false))))))))

(defn slow [pattern-data query-arc]
  (fast (update pattern-data :speed #(/ 1 %))
        query-arc))

(defmethod query :fast
  [pattern-data query-arc]
  (fast pattern-data query-arc))

(defmethod query :slow
  [pattern-data query-arc]
  (slow pattern-data query-arc))

(defmethod query :speed
  [{:keys [value speedable-pattern]} query-arc]
  (mapcat (fn [{:keys [value arc/active]}]
            (query (assoc speedable-pattern :speed value)
                   active))
          (query value query-arc)))

(defn wrap-nth
  [coll index]
  (nth coll (mod index (count coll))))

(defn slowcat
  [{:keys [value]} query-arc]
  (mapcat (fn [arc]
            (let [v (wrap-nth value (first arc))
                  events (query v arc)]
              events))
          (span-cycles query-arc)))

(defmethod query :slowcat
  [data query-arc]
  (slowcat data query-arc))

(do
  (defn slowcat2
    [{:keys [value len]} query-arc]
    (let [[start end ] query-arc]
      (loop [start* (int start)
             events []]
        (let [i (mod start* len)
              cycle (quot start* len)
              value* (wrap-nth value (int i))
              ratio (:ratio value* 1)
              end* (+ ratio start*)
              new-events (concat
                           events
                           (->> (query value* [cycle (inc cycle)])
                                (map #(update % :arc/active
                                              (fn [[s e]]
                                                [(+ s (- start* cycle))
                                                 ;; FIXME could improve because here the event ends before it should when ratio > 1
                                                 (+ e (- start* cycle))])))
                                (filter #(-> %
                                             :arc/active
                                             first
                                             (>= start)))))]
          (if (> end* end)
            new-events
            (recur end* new-events )))))

    )

  (map (juxt :value :arc/active (comp nil? :has-start?))
       (slowcat2 {:pattern/type :slowcat
                  :len 3
                  :value [{:pattern/type :fastcat
                           :value [{:pattern/type :atom :value "bd" :ratio 1}
                                   {:pattern/type :atom :value "bd" :ratio 1}]}
                          {:pattern/type :slow
                           :ratio 2
                           :speed 2
                           :value {:pattern/type :atom :value "hh"}}
                          #_{:pattern/type :atom :value "hh" :ratio 2}]}
                 [0 10])))

(query {:pattern/type :slow
        :speed 2
        :value {:pattern/type :atom :value
                "hh"}}
       [1 2])

(defmethod query :fastcat
  [data query-arc]
  (let [pats (:value data)]
    (fast {:speed (count pats)
           :value {:pattern/type :slowcat
                   :value pats}}
          query-arc)))

#_(do
    (defmethod query :fast-gap
      [{:keys [speed value] :as  data} query-arc]
      (mapcat (fn [query-arc]
                (-> value
                    (with-query-time #(* % 1) query-arc)
                    (->> (map (fn [ev]
                                (let [ev* (update-event-time ev #(/ % 1))]
                                  (if (<= (first query-arc) (first (:arc/active ev*)))
                                    ev*
                                    (assoc ev* :has-start? false))))))))
              (span-cycles query-arc))
      #_(query value query-arc)
      #_(reduce (fn [acc {:keys [arc/active] :as ev}]
                  (if (< (mod (first active) 1)
                         (/ 1 speed))
                    (conj acc ev)
                    acc))
                []
                (fast data query-arc)))
    (query {:pattern/type :fast-gap
            :speed 2
            :value {:pattern/type :fastcat
                    :value [{:pattern/type :slowcat
                             :value [{:pattern/type :atom :value "bd"}
                                     {:pattern/type :atom :value "sd"}]}
                            {:pattern/type :atom :value "hh"}
                            {:pattern/type :atom :value "cp"}]}}
           [1 2]))

(defmethod query :rotl
  [{:keys [value amount]} query-arc]
  (-> value
      (with-query-time #(+ % amount) query-arc)
      (->> (map (fn [ev] (update-event-time ev #(- % amount)))))))

(defmethod query :rotr
  [{:keys [amount] :as data} query-arc]
  (query (assoc data
                :pattern/type :rotl
                :amount (* -1 amount))
         query-arc))

(comment
  (query {:pattern/type :rotl
          :amount -1/4
          :value {:pattern/type :fastcat
                  :value [{:pattern/type :atom :value 1}
                          {:pattern/type :atom :value 2}]}}
         [0 1])
  (query {:pattern/type :rotr
          :amount 1/3
          :value {:pattern/type :fastcat
                  :value [{:pattern/type :atom :value 1}
                          {:pattern/type :atom :value 2}
                          #_{:pattern/type :atom :value 3}
                          #_{:pattern/type :atom :value 4}]}}
         [0 2]))

(defn starts-in-cycle?
  [cycle [arc-start]]
  (<= cycle arc-start))

(defn ends-in-cycle?
  [cycle [_ arc-end]]
  (and (< cycle arc-end)
       (<= arc-end (inc cycle))))

(defn arc-length
  [[start end]]
  (- end start))

(defn mirror-point
  "Given a point from an arc (start or end) in a cycle, return the position at the other side of the cycle's middle"
  [cycle point]
  (let [middle (+ 1/2 cycle)]
    (+ middle (- middle point))))

(defn rev-event
  [current-cycle event]
  (let [silence? (ends-in-cycle? current-cycle (:arc/whole event))
        reverse-arc (fn [[start end]]
                      (let [start* (mirror-point current-cycle end)
                            end* (mirror-point current-cycle start)]
                        [start* end*]))]
    (-> event
        (update :arc/whole reverse-arc)
        (update :arc/active reverse-arc)
        (dissoc :has-start?)
        (cond-> (not silence?) (assoc :value :silence
                                      :original-value (:value event))))))

(defmethod query :rev
  [{:keys [value]} query-arc]
  (mapcat (fn [query-arc*]
            (reverse (map (partial rev-event (first query-arc*))
                          (query value query-arc*))))
          (span-cycles query-arc)))

(defn silence?
  [event]
  (or (= :silence (:value event))
      (not (:has-start? event true))))

(defn remove-silences [events]
  (remove silence? events))

(do
  (defn odd-cycle? [cycle-arc]
    (odd? (first cycle-arc)))

  (defn palindrome-cycles
    "Slow down the query-arc to repeat each cycle twice so that the odd one can be reversed"
    [query-arc]
    (map (fn [cycle-arc]
           (let [cycle (first cycle-arc)
                 start (int (/ cycle 2))]
             {:cycle cycle :arc [start (inc start)]}))
         (span-cycles query-arc))))

(defn transpose-back-palindrome-event
  [cycle ev]
  (update ev :arc/active
          (fn [[start end]]
            (if (even? cycle)
              (let [start* (* 2 start)]
                [start* (+ start* (- end start))])
              (let [start* (inc (* 2 start))]
                [start* (+ start* (- end start))])))))

(defmethod query :palindrome
  [data query-arc]
  (mapcat (fn [{:keys [cycle arc]}]
            (map (partial transpose-back-palindrome-event cycle)
                 (if (even? cycle)
                   (query (:value data) arc)
                   (query {:pattern/type :rev
                           :value (:value data)}
                          arc))))
          (palindrome-cycles query-arc)))
