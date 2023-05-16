(ns tidal-mini.query2
  (:require
   [clojure.math :as math]
   [tidal-mini.euclidean-rhythm :refer [euclidean-rhythm]]
   [tidal-mini.time :refer [apply-pat-to-pat-both]]
   [tidal-mini.utils :refer [rotate]]))

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

(def event-summary (juxt :value :arc/active (comp nil? :has-start?)))

(do
  (defn filter-events-in-arc
    [[arc-start arc-end] events]
    (filter (fn [{[start] :arc/active}]
              (and (>= start arc-start)
                   (< start arc-end)))
            events))
  (declare split-cycles transpose-events-into-arc)

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
  (comment)
  (query {:pattern/type :fastgap
          :speed 2
          :value {:pattern/type :fastcat
                  :len 3
                  :value [{:pattern/type :atom :value "bd"}
                          {:pattern/type :atom :value "hh"}
                          {:pattern/type :slowcat
                           :len 2
                           :value [{:pattern/type :atom :value "cp"}
                                   {:pattern/type :atom :value "sd"}]}]}}
         [1/6 4/3]))

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

(comment
  (map (juxt :value :arc/active)
       (remove-silences
        (query {:pattern/type :*
                :op-pattern {:pattern/type :fastcat
                             :len 2
                             :value [{:pattern/type :atom :value/type :note :value 1}
                                     {:pattern/type :atom :value/type :note :value 1}]}
                :value {:pattern/type :fastcat
                        :len 3
                        :value [{:pattern/type :atom :value 1 :value/type :note}
                                {:pattern/type :atom :value 2 :value/type :note}
                                {:pattern/type :slowcat
                                 :len 2
                                 :value [{:pattern/type :atom :value 3 :value/type :note}
                                         {:pattern/type :atom :value 4 :value/type :note}]}]}}
               [0 4]))))

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
            valid-onset? (fn [{[onset] :arc/active}]
                           (and (>= onset start)
                                (>= onset start*)))
            new-events (concat
                        events
                        (->> (query value* [cycle (inc cycle)])
                             (map (update-arc :arc/active))
                              ;; FIXME this should be something else altogether
                             (map (update-arc :arc/whole))
                             (filter valid-onset?)))]

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

(query
 {:pattern/type :fastcat
  :len 3
  :value
  [{:pattern/type :atom, :value "bd"}
   {:pattern/type :atom, :value "cp"}]}
 [0 1])

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

(sort-by (comp first :arc/active)
         (query {:pattern/type :polymeter
                 :len 2
                 :value [[{:pattern/type :atom, :value "bd" :value/type :sound}
                          {:pattern/type :atom, :value "cp" :value/type :sound}]
                         [{:pattern/type :slowcat
                           :len 2
                           :value [{:pattern/type :atom, :value "a" :value/type :sound}
                                   {:pattern/type :atom, :value "A" :value/type :sound}]}
                          {:pattern/type :atom, :value "b" :value/type :sound}
                          {:pattern/type :atom, :value "c" :value/type :sound}]]}
                [1/2 2]))

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

(map (juxt :value :arc/active)
     (query {:pattern/type :euclidean
             :pulses 3
             :steps 8
             :rotation 0
             :value {:pattern/type :slowcat
                     :len 2
                     :value [{:pattern/type :fastcat
                              :len 2
                              :value [{:pattern/type :atom, :value "cp" :value/type :sound}
                                      {:pattern/type :slow
                                       :speed 2
                                       :value {:pattern/type :atom, :value "sd" :value/type :sound}}]}
                             {:pattern/type :atom, :value "bd" :value/type :sound}]}}
            [0 3]))

(defmethod query :layer
  [{:keys [value fs]} query-arc]
  (query {:pattern/type :stack
          :value (map (fn [f] (f value)) fs)}
         query-arc))
(query {:pattern/type :layer
        :fns [(fn [v] {:pattern/type :rev :value v})
              (fn [v] {:pattern/type :fast :speed 3 :value v})]
        :value {:pattern/type :fastcat
                :len 2
                :value [{:pattern/type :atom :value "bd"}
                        {:pattern/type :atom :value "cp"}]}}
       [0 1])

(defmethod query :pan
  [{:keys [value pan]} query-arc]
  (->> query-arc
       (query value)
       (map #(assoc % :pan pan))))

(query {:pattern/type :pan
        :pan -1
        :value {:pattern/type :fastcat
                :len 2
                :value [{:pattern/type :atom :value "bd"}
                        {:pattern/type :atom :value "cp"}]}}
       [0 1])

(defmethod query :jux
  [{:keys [value f]} query-arc]
  (query {:pattern/type :layer
          :fns [(fn [v] {:pattern/type :pan
                         :pan -1
                         :value (f v)})
                (fn [v] {:pattern/type :pan
                         :pan 1
                         :value v})]
          :value value}
         query-arc))

(query {:pattern/type :jux
        :f (fn [v] {:pattern/type :fast :speed 3 :value v})
        :value {:pattern/type :fastcat
                :len 2
                :value [{:pattern/type :atom :value "bd"}
                        {:pattern/type :atom :value "cp"}]}}
       [0 1/2])

(defn split-cycles
  [[start end]]
  (loop [start* start
         end* (min end (int (math/ceil start)))
         arcs []]
    (let [arcs* (if (= start* end*)
                  arcs
                  (conj arcs [start* end*]))]
      (cond
        (= end end*) arcs*
        :else (recur end* (min end (inc end*)) arcs*)))))

(defmethod query :silence
  [_ query-arc]
  (query {:pattern/type :atom :value :silence}
         query-arc))

(defn transpose-events-into-arc
  [{:keys [origin-arc target-arc events]}]
  (if-not (seq events)
    ()
    (let [[start-pos end-pos] origin-arc
          [start end] target-arc
          arc-ratio (- end start)
          sorted-events (sort-by (comp  first :arc/active) events)
          events-arc-ratio (- end-pos start-pos)]
      (map (fn [{:as ev
                 [s e] :arc/active}]
             (let [dur (/ (* arc-ratio (- e s)) events-arc-ratio)
                   offset (+ start (/ (* arc-ratio (- s start-pos)) events-arc-ratio))
                   new-arc [offset (+ offset dur)]]
               (assoc ev :arc/active new-arc
                      :arc/whole new-arc)))
           sorted-events))))

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
  ;; NOTE this might not work as expected, implementation is very different from actual Tidal
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

(comment
  (declare remove-silences)
  (->> [0 10]
       (query {:pattern/type :sometimes-by
               :probability 0.5

               :f (fn [v] {:pattern/type :pan :pan 1 :value v})
               ;; :f (fn [_] {:pattern/type :silence})
               ;; :f (fn [v] {:pattern/type :fast :speed 2 :value v})
               :value {:pattern/type :fastcat
                       :len 2
                       :value [{:pattern/type :atom :value "bd"}
                               {:pattern/type :atom :value "cp"}]}})
       remove-silences
       (sort-by (comp first :arc/active))
       (map (juxt :value :arc/active :pan))))

(query {:pattern/type :degrade-by
        :value {:pattern/type :atom :value "cp"}
        :probability 0.5}
       [0 10])

(query {:pattern/type :degrade
        :value {:pattern/type :fastcat
                :len 2
                :value [{:pattern/type :atom :value "bd"}
                        {:pattern/type :atom :value "cp"}]}
        :probability 0.5}
       [0 10])

(query  {:pattern/type :fastcat
         :len 2
         :value [{:pattern/type :degrade-by
                  :value {:pattern/type :fastcat
                          :len 2
                          :value [{:pattern/type :atom :value "bd"}
                                  {:pattern/type :atom :value "cp"}]}
                  :probability 0.5}
                 {:pattern/type :atom :value "hh"}]}
        [0 10])

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

(query {:pattern/type :fast
        :speed 2
        :value {:pattern/type :atom :value "cp", :arc/whole [9/2 5], :arc/active [9/2 5]}}
       [9/2 5])

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

(comment (query {:pattern/type :somecycles-by
                 :probability 0.5
                 :f (fn [_] {:pattern/type :silence})
                 :value {:pattern/type :fastcat
                         :len 2
                         :value [{:pattern/type :atom :value "bd"}
                                 {:pattern/type :atom :value "cp"}]}}
                [0 10]))

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
                  :len 3
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
  [{:as event
    [start end] :arc/active}]
  (or (= :silence (:value event))
      (not (:has-start? event true))
      (= start end)))

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
