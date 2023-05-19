(ns piratidal.pattern
  (:require
   [piratidal.euclidean-rhythm :refer [euclidean-rhythm]]
   [piratidal.time
    :refer [apply-pat-to-pat-both
            ends-in-cycle?
            next-sam
            sam
            span-cycles
            split-cycles
            transpose-events-into-arc
            update-event-time
            update-span-time]]
   [piratidal.utils :refer [rotate wrap-nth]]))

(defmulti query
  (fn [data _query-arc] (:pattern/type data :event)))

(defmethod query :atom
  [{:keys [value value/type]} query-arc]
  (map (fn [active]
         {:value/type type
          :value value
          :arc/whole [(sam (first active)) (next-sam (first active))]
          :arc/active active})
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

(defmethod query :speed
  [{:keys [value speedable-pattern]} query-arc]
  (mapcat (fn [{:keys [value arc/active]}]
            (query (assoc speedable-pattern :speed value)
                   active))
          (query value query-arc)))

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

(defn make-apply-query
  ;; NOTE because of the impl of `apply-pat-to-pat-both`, this may return 0-length events. Which will be considered as silences by the `silence?` fn.
  [op op-pattern value query-arc]
  (let [events (query value query-arc)
        op-events (query op-pattern query-arc)]
    (apply-pat-to-pat-both op events op-events)))

(defmethod query :+
  [{:keys [value op-pattern]} query-arc]
  (make-apply-query + op-pattern value query-arc))

(defmethod query :-
  [{:keys [value op-pattern]} query-arc]
  (make-apply-query - op-pattern value query-arc))

(defmethod query :*
  [{:keys [value op-pattern]} query-arc]
  (make-apply-query * op-pattern value query-arc))

(defmethod query :div
  ;; NOTE: using `:div` instead of `:/` because cljfmt doesn't like that name and it's probably not the nicest keyword anyways as it "conflicts" with namespaced keywords
  [{:keys [value op-pattern]} query-arc]
  ;; prevent division by `0` by returning `0` even if it's not mathematically correct ¯\_(ツ)_/¯
  (let [safe-div (fn [a b] (if (zero? b) 0 (/ a b)))]
    (make-apply-query safe-div op-pattern value query-arc)))

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
  [{:keys [value fs]} query-arc]
  (query {:pattern/type :stack
          :value (map (fn [f] (f value)) fs)}
         query-arc))

(defmethod query :jux/pan
  ;; TODO eventually remove this for a more general solution
  [{:keys [value pan]} query-arc]
  (->> (query value query-arc)
       (map #(assoc % :pan pan))))

(defmethod query :jux
  [{:keys [value f]} query-arc]
  (query {:pattern/type :layer
          :fs [(fn [v] {:pattern/type :jux/pan
                        :pan -1
                        :value (f v)})
               (fn [v] {:pattern/type :jux/pan
                        :pan 1
                        :value v})]
          :value value}
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
       (map (fn [ev]
              (if (> (rand-fn ev) probability)
                ev
                (assoc ev :value :silence))))))

(defmethod query :degrade
  [data query-arc]
  (query (assoc data :pattern/type :degrade-by :probability 0.5)
         query-arc))

(defmethod query :undegrade-by
  [{:keys [value probability rand-fn]
    :or {rand-fn (fn [_event] (rand))}} query-arc]
  (->> query-arc
       (query value)
       (map (fn [ev]
              (if (>= probability (rand-fn ev))
                ev
                (assoc ev :value :silence))))))

(defmethod query :sometimes-by
  [{:keys [value probability f]} query-arc]
  (let [rand-fn (memoize (fn [_] (rand)))]
    (concat (query {:pattern/type :degrade-by
                    :rand-fn rand-fn
                    :probability probability
                    :value value}
                   query-arc)
            (query (f {:pattern/type :undegrade-by
                       :rand-fn rand-fn
                       :probability probability
                       :value value})
                   query-arc))))

(defmethod query :sometimes
  [data query-arc]
  (query (make-sometimes-by 1/2 data) query-arc))

(defmethod query :often
  [data query-arc]
  (query (make-sometimes-by 3/4 data) query-arc))

(defmethod query :rarely
  [data query-arc]
  (query (make-sometimes-by 1/4 data) query-arc))

(defmethod query :almost-always
  [data query-arc]
  (query (make-sometimes-by 9/10 data) query-arc))

(defmethod query :almost-never
  [data query-arc]
  (query (make-sometimes-by 1/10 data) query-arc))

(defmethod query :somecycles-by
  [{:keys [probability value f]} query-arc]
  (mapcat
   (fn [arc]
     (if (> probability (rand))
       (query (f value) arc)
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


;;;;;; Control Patterns


(defn event-onset [event]
  (-> event :arc/active first))

(defn sort-events-by-onset [events]
  (sort-by event-onset events))

(defn assoc-control
  [event {:keys [value/type value] :as _ctl-event}]
  (update event type (fn [prev-val] (if prev-val prev-val value))))

(defn update-control-pattern-event
  "`new-events` are the events being updated, `events` are the remaining events to be updated"
  [{:keys [new-events event ctl-event events]}]
  (let [new-events (assoc new-events
                          (dec (count new-events))
                          (assoc-control event ctl-event))
        next-event (first events)]
    (if next-event
      (conj new-events next-event)
      new-events)))

(defn event-has-current-ctl-event-value?
  [event ctl-event] (= (:value ctl-event)
                       ((:value/type ctl-event) event)))

(defn control-pattern-query-done?
  [{:keys [events ctl-events ctl-events-list]}]
  (and (not (seq events))
       (not (seq ctl-events))
       (not (seq ctl-events-list))))

(defn control-pattern-query-single-cycle
  ;; FIXME this can probably be improved by a lot
  [{:keys [value controls]} query-arc]
  (let [ctl-events-list (map #(sort-events-by-onset (query % query-arc)) controls)
        events (sort-events-by-onset (query value query-arc))
        data {:new-events [(first events)]
              :events (rest events)
              :ctl-events-list (rest ctl-events-list)
              :ctl-events (first ctl-events-list)}]

    (loop [{:keys [new-events events ctl-events-list ctl-events] :as data} data]
      (let [ctl-event (first ctl-events)
            event (last new-events)
            ctl-onset (event-onset ctl-event)
            next-ctl-onset (event-onset (second ctl-events))
            event-onset* (event-onset event)
            next-ctl-events-set? (not (seq ctl-events))]
        (cond
          (control-pattern-query-done? data) new-events

          next-ctl-events-set?
          (recur {:new-events [(first new-events)]
                  :events (rest new-events)
                  :ctl-events-list (rest ctl-events-list)
                  :ctl-events (first ctl-events-list)})

          (event-has-current-ctl-event-value? event ctl-event)
          (recur (assoc data :ctl-events (rest ctl-events)))

          (or (and next-ctl-onset (<= next-ctl-onset event-onset*))
              (> ctl-onset event-onset*))
          (recur (assoc data :ctl-events (rest ctl-events)))

          (<= ctl-onset event-onset*)
          (recur (assoc data
                        :new-events (update-control-pattern-event
                                     (assoc data
                                            :event event
                                            :ctl-event ctl-event))
                        :events (rest events))))))))

(defmethod query :control-pattern
  [data query-arc]
  (mapcat (fn [query-arc]
            (control-pattern-query-single-cycle data query-arc))
          (split-cycles query-arc)))

(defn in-arc [[start end] onset-point]
  (and (>= onset-point start)
       (< onset-point end)))

#_(defn inner-join
    [f outer-events inner-events]
    (->> (for [o-event outer-events
               {[i-start] :arc/active :as i-event} inner-events]
           (when (in-arc (:arc/active o-event) i-start)
             (f o-event i-event)))
         (remove nil?)))

(defmethod query :superimpose
  [{:keys [value f]} query-arc]
  (query {:pattern/type :stack
          :value [value (f value)]}
         query-arc))

(defmethod query :off
  [{:keys [amount value f]} query-arc]
  (query {:pattern/type :superimpose
          :value value
          :f (fn [value*] (f {:pattern/type :rotr
                              :amount amount
                              :value value*}))}
         query-arc))


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
  [{:keys [value op op-patterns value-type pattern-constructor apply-structure]
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
