(ns piratidal.pattern
  (:require
   [piratidal.euclidean-rhythm :refer [euclidean-rhythm]]
   [piratidal.parser :refer [maybe-parse-pattern with-param-pattern]]
   [piratidal.time
    :refer [apply-pat-to-pat-both apply-pat-to-pat-left ends-in-cycle?
            event->param-map merge-pat-to-pat-left next-sam sam span-cycles
            split-cycles transpose-events-into-arc update-event-time update-span-time]]
   [piratidal.utils :refer [rotate wrap-nth]]))

(defmulti query
  (fn [data _query-arc] (:pattern/type data :event)))

(defmethod query :atom
  [data query-arc]
  (map (fn [active]
         (-> data
             (dissoc :pattern/type)
             (assoc :arc/whole [(sam (first active)) (next-sam (first active))]
                    :arc/active active)))
       (span-cycles query-arc)))

(defmethod query :event
  ;; NOTE This might not be the best thing to do as an event is technically not a pattern
  [data _query-arc]
  data)

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

;; the tParam familiy in tidal, except for squeezeParam
(defmethod query :with-param-pattern
  [{:keys [pattern/params value]} query-arc]
  (->> params
       (map #(query % query-arc))
       (reduce (fn [apped-events new-events]
                 (apply-pat-to-pat-both + apped-events new-events)))
       (mapcat (fn [{:keys [arc/active] :as event}]
                 (query (merge value (event->param-map event))
                        active)))))

(defn filter-events-in-arc
  [[arc-start arc-end] events]
  (filter (fn [{[start] :arc/active}]
            (and (>= start arc-start)
                 (< start arc-end)))
          events))

(defmethod query :fastgap
  [{:keys [value speed]} query-arc]
  (->> (split-cycles query-arc)
       (mapcat (fn [[start end]]
                 (let [cycle (int start)
                       target-arc-end  (+ cycle (/ 1 speed))
                       target-arc [cycle target-arc-end]
                       origin-arc [cycle (inc cycle)]
                       events (query value origin-arc)]
                   (filter-events-in-arc
                    [start (min end target-arc-end)]
                    (transpose-events-into-arc
                     {:origin-arc origin-arc
                      :target-arc target-arc
                      :events events})))))))

(defmethod query :timecat
  [{:keys [value]} query-arc]
  (let [total (apply + (map first value))])
  ;; TODO
  )
(defn slowcat
  ;; TODO, test how this works when starting at spans different from 0 and perhaps a few cycles afterwards
  ;; FIXME probably elongate still doesn't work as expected
  [{:keys [value len]} query-arc]

  (let [[start end] query-arc]
    (loop [start* (int start)
           events []]
      (let [i (mod start* len)
            cycle (quot start* len)
            value* (wrap-nth value (int i))
            ratio (:ratio value* 1)
            end* (+ ratio start*)
            update-arc (fn [arc-k] #(update % arc-k
                                            (fn [[s e]] [(+ s (- start* cycle))
                                                       ;; FIXME could improve because here the event ends before it should when ratio > 1
                                                         (+ e (- start* cycle))])))
            evaluate-onset-time (fn [{:as event
                                      [onset] :arc/active}]
                                  (let [has-start? (and (>= onset start)
                                                        (>= onset start*))]
                                    (if has-start?
                                      event
                                      (assoc event :has-start? false))))
            new-events (concat
                        events
                        (->> (query value* [cycle (inc cycle)])
                             (map (update-arc :arc/active))
                              ;; FIXME this should be something else altogether
                             (map (update-arc :arc/whole))
                             (map evaluate-onset-time)))]

        (cond
          (>= start* end) events
          (> end* end) new-events
          :else (recur end* new-events))))))

(defmethod query :slowcat
  [data query-arc]
  (assert (:len data) ":len must be present")
  (slowcat data query-arc))

(defmethod query :cat
  [data query-arc]
  (query (assoc data :pattern/type :slowcat) query-arc))

(defmethod query :fastcat
  [{:keys [len] :as data} query-arc]
  (let [pats (:value data)]
    (fast {:speed (count pats)
           :value {:pattern/type :slowcat
                   :len len
                   :value pats}}
          query-arc)))

(defmethod query :stack
  [data query-arc]
  (->> data :value
       (mapcat #(query % query-arc))
       ;; sorting may not be necessary, but it makes testing easier
       #_(sort-by (comp first :arc/active))))

(defmethod query :polymeter
  ;; NOTE: value is actually a vector of pattern vectors
  [{:keys [value len]} query-arc]
  (let [query-value (fn [arc index value*]
                      (query {:pattern/type :fastcat
                              :len len
                              :value (map #(wrap-nth value* %)
                                          (range index (+ len index)))}
                             arc))]
    (->> (split-cycles query-arc)
         (mapcat (fn [arc]
                   (let [[start] arc
                         index (* (int start) len)]
                     (mapcat #(query-value arc index %) value)))))))

(defmethod query :euclidean
  [{:keys [value pulses steps rotation]
    :or {rotation 0}}
   query-arc]
  (query {:pattern/type :fastcat
          :len steps
          :value (map (fn [pulse]
                        (if (zero? pulse)
                          {:pattern/type :atom, :value :silence :value/type :sound}
                          value))
                      (rotate (euclidean-rhythm pulses steps) rotation))}
         query-arc))

(defmethod query :layer
  [{:keys [value]} query-arc]
  ;; NOTE this is different from tidal's implementation as the mapping/application of the functions is done at compile time rather than on the fly, for performance reasons, hence `layer` is basically `stack`
  (query {:pattern/type :stack
          :value value}
         query-arc))

(defmethod query :jux/pan
  ;; TODO eventually remove this for a more general solution
  [{:keys [value pan]} query-arc]
  (->> (query value query-arc)
       (map #(assoc % :pan pan))))

(defmethod query :jux
  [{:keys [value fvalue]} query-arc]
  (query {:pattern/type :layer
          :value [{:pattern/type :jux/pan
                   :pan 0
                   :value fvalue}
                  {:pattern/type :jux/pan
                   :pan 1
                   :value value}]}
         query-arc))

(defmethod query :silence
  [_ query-arc]
  (query {:pattern/type :atom :value :silence}
         query-arc))

(defn make-sometimes-by
  [probability data]
  (merge data {:pattern/type :somecycles-by
               :probability probability}))

(defmethod query :degrade-by
  [{:keys [value probability rand-fn]
    :or {rand-fn (fn [_event] (rand))}} query-arc]
  (->> query-arc
       (query value)
       (reduce (fn [acc ev]
                 (if (> (rand-fn ev) probability)
                   (conj acc ev)
                   acc))
               [])))

(defmethod query :degrade
  [data query-arc]
  (query (assoc data :pattern/type :degrade-by :probability 0.5)
         query-arc))

(defmethod query :undegrade-by
  [{:keys [value probability rand-fn]
    :or {rand-fn (fn [_event] (rand))}} query-arc]
  (->> query-arc
       (query value)
       (reduce (fn [acc ev]
                 (if (>= probability (rand-fn ev))
                   (conj acc ev)
                   acc))
               [])))

(defn sometimes-by
  [value probability f]
  (let [rand-fn (memoize (fn [_] (rand)))]
    {:pattern/type :sometimes-by
     :value (with-param-pattern
              [(maybe-parse-pattern
                probability {:value-type :probability})]
              {:pattern/type :degrade-by
               :rand-fn rand-fn
               :value value})
     :fvalue (f (with-param-pattern
                  [(maybe-parse-pattern
                    probability {:value-type :probability})]
                  {:pattern/type :undegrade-by
                   :rand-fn rand-fn
                   :value value}))}))

(defn make-sometimes-fn
  [probability]
  (fn [value f] (sometimes-by value probability f)))

(defmethod query :sometimes-by
  [{:keys [value fvalue]} query-arc]
  (query {:pattern/type :stack
          :value [value fvalue]}
         query-arc))

(def sometimes (make-sometimes-fn 1/2))
(def often (make-sometimes-fn 3/4))
(def rarely (make-sometimes-fn 1/4))
(def almost-always (make-sometimes-fn 9/10))
(def almost-never (make-sometimes-fn 1/10))

(defmethod query :somecycles-by
  [{:keys [probability value fvalue]} query-arc]
  (mapcat
   (fn [arc]
     (if (> probability (rand))
       (query fvalue arc)
       (query value arc)))
   (split-cycles query-arc)))

(defmethod query :somecycles
  [data query-arc]
  (query (make-sometimes-by 1/2 data) query-arc))

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
  [{:as event
    [start end] :arc/active}]
  (or (= :silence (:value event))
      (not (:has-start? event true))
      (= start end)))

(defn remove-silences [events]
  (remove silence? events))

(defn odd-cycle? [cycle-arc]
  (odd? (first cycle-arc)))

(defn palindrome-cycles
  "Slow down the query-arc to repeat each cycle twice so that the odd one can be reversed"
  [query-arc]
  (map (fn [cycle-arc]
         (let [cycle (first cycle-arc)
               start (int (/ cycle 2))]
           {:cycle cycle :arc [start (inc start)]}))
       (span-cycles query-arc)))
(do
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
              (let [[start] arc]
                (if (even? cycle)
                  (transpose-events-into-arc
                   {:origin-arc arc
                    :target-arc [(* 2 start) (inc (* 2 start))]
                    :events (query (:value data) arc)})
                  (transpose-events-into-arc
                   {:origin-arc arc
                    :target-arc [(inc (* 2 start)) (inc (inc (* 2 start)))]
                    :events (query {:pattern/type :rev
                                    :value (:value data)}
                                   arc)}))))
            (palindrome-cycles query-arc)))

  (mapv (juxt :value :arc/active)
        (remove-silences
         (query {:pattern/type :palindrome
                 :value {:pattern/type :with-param-pattern
                         :value {:pattern/type :fast
                                 :value {:pattern/type :fastcat
                                         :len 3
                                         :value [{:pattern/type :atom, :value "bd", :value/type :s}
                                                 {:pattern/type :atom, :value "cp", :value/type :s}
                                                 {:pattern/type :atom, :value "hh", :value/type :s}]}}
                         :pattern/params [{:pattern/type :atom, :value 2, :value/type :speed}]}}
                [0 2]))))

;;;;;; Control Patterns


(defn control-pattern-query-single-cycle
  [{:keys [value controls]} query-arc]
  (let [ctl-events-list (map #(query % query-arc) controls)
        events (query value query-arc)]
    (reduce (fn [events ctl-events]
              (merge-pat-to-pat-left events ctl-events))
            events
            ctl-events-list)))

(defmethod query :control-pattern
  [data query-arc]
  (mapcat (fn [query-arc]
            (control-pattern-query-single-cycle data query-arc))
          (split-cycles query-arc)))

(defmethod query :superimpose
  [{:keys [value fvalue]} query-arc]
  (query {:pattern/type :stack
          :value [value fvalue]}
         query-arc))

(defn apply-off-fn
  [f amount value]
  ;; FIXME this should be patternizable
  (f (with-param-pattern
       [(maybe-parse-pattern amount {:value-type :amount})]
       {:pattern/type :rotr
        :value value})))

;; FIXME with-param-pattern no longer works now, see above
(defmethod query :off
  [{:keys [value fvalue]} query-arc]
  ;; NOTE as is the case with the patterns that take another function, the function call happens at compile time, see the call-site of `apply-off-fn'
  (query {:pattern/type :superimpose
          :value value
          :fvalue fvalue}
         query-arc))

(query
 {:pattern/type :with-param-pattern
  :value {:pattern/type :off
          :value {:pattern/type :fastcat
                  :len 1
                  :value [{:pattern/type :atom, :value "bd", :value/type :s}]}
          :fvalue {:pattern/type :control-pattern
                   :value {:pattern/type :rotr
                           :amount 1/4
                           :value {:pattern/type :fastcat
                                   :len 1
                                   :value [{:pattern/type :atom, :value "bd", :value/type :s}]}}
                   :controls [{:pattern/type :fastcat
                               :len 1
                               :value [{:pattern/type :atom, :value 4, :value/type :crush}]}]}}
  :pattern/params [{:pattern/type :fastcat
                    :len 1
                    :value [{:pattern/type :atom, :value 1/4, :value/type :amount}]}]}
 [1 2])

;;;;;;; Math


(defn make-apply-query
  ;; NOTE because of the impl of `apply-pat-to-pat-both`, this may return 0-length events. Which will be considered as silences by the `silence?` fn.
  [{:keys [op op-pattern value op-direction query-arc]}]
  (let [events (query value query-arc)
        op-events (query op-pattern query-arc)
        apply-fn (case op-direction
                   :both apply-pat-to-pat-both
                   :left apply-pat-to-pat-left)]
    (apply-fn op events op-events)))

(defmethod query :math-op
  [{:keys [value op op-patterns apply-structure]
    :or {apply-structure :both}} query-arc]
  (let [events (if (seq op-patterns)
                 (reduce (fn [value op-pattern]
                           (make-apply-query {:op op
                                              :op-pattern op-pattern
                                              :value value
                                              :op-direction apply-structure
                                              :query-arc query-arc}))
                         value
                         op-patterns)
                 (query value query-arc))]
    events))
