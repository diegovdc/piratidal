(ns piratidal.time
  (:require
   [clojure.math :as math]))

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

(defn sam [n] (int n))
(defn next-sam [n] (inc (sam n)))

(defn update-span-time
  [timef arc]
  (map timef arc))

(defn update-event-time
  [{:as event :keys [arc/active arc/whole]} timef]
  (assoc event
         :arc/whole (mapv timef whole)
         :arc/active (mapv timef active)))

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

(defn sect
  "Simple intersection of two arcs
  From Sound.Tidal.Time:"
  [[start end] [start* end*]]
  [(max start start*) (min end end*)])

(defn sub-arc
  "NOTE: This function might return `nil` or an arc
  Taken from Sound.Tidal.Time:
  -- | @subArc i j@ is the timespan that is the intersection of @i@ and @j@.
  -- intersection
  -- The definition is a bit fiddly as results might be zero-width, but
  -- not at the end of an non-zero-width arc - e.g. (0,1) and (1,2) do
  -- not intersect, but (1,1) (1,1) does."
  [arc-1 arc-2]
  (let [[start end] (sect arc-1 arc-2)]
    (when (<= start end) [start end])))

(defn apply-op-event
  [op main-event op-event]
  (let [op-type (:value/type op-event)
        op-val (:value op-event)
        main-type (:value/type main-event)
        main-val (if (= op-type main-type)
                   (:value main-event)
                   (op-type main-event))
        new-val (if main-val
                  (op main-val op-val)
                  op-val)]
    (if (= op-type main-type)
      (assoc main-event :value new-val)
      (assoc main-event op-type new-val))))

(comment
  (apply-op-event +
                  {:value/type :note :value 1}
                  {:value/type :note :value 2}))

(defn apply-pat-to-pat-both
  ;; NOTE: wholes are note being dealed with at the moment. FIXME
  "From Sound.Tidal.Pattern definition of  Applicative Pattern:
  -- | In each of `a <*> b`, `a <* b` and `a *> b`
  -- (using the definitions from this module, not the Prelude),
  -- the time structure of the result
  -- depends on the structures of both `a` and `b`.
  -- They all result in `Event`s with identical `part`s and `value`s.
  -- However, their `whole`s are different.
  "
  [op main-events op-events]
  (->> (for [{arc :arc/active :as main-event} main-events
             {arc* :arc/active :as op-event} op-events]
         (let [arc-intersection (sub-arc arc arc*)]
           ;; IMPORTANT NOTE tidal doesn't filter out zero width queries, so this might be problematic
           (when (and arc-intersection (apply not= arc-intersection))
             (-> (apply-op-event op main-event op-event)
                 (assoc :arc/active arc-intersection)))))
       (remove nil?)))

(defn event->param-map
  "Remove :value, :value/type, :arc/active and :arc/whole  and set value as a simple param of the event"
  [event]
  (-> event
      (dissoc :value :value/type :arc/active :arc/whole)
      (assoc (:value/type event) (:value event))))

(comment
  (event->param-map {:value/type :pulse :value 2 :step 4
                     :arc/active [0 1/3]}))

(defn merge-params
  [main-event op-event]
  (merge main-event (event->param-map op-event)))

(defn merge-pat-to-pat-left
  ([main-events op-events] (merge-pat-to-pat-left merge-params main-events op-events))
  ([merge-fn main-events op-events]
   (->> (for [{[start _end] :arc/active :as main-event} main-events
              {[start* end*] :arc/active :as op-event} op-events]
          (when (and (>= start start*)
                     (< start end*))
            (merge-fn main-event op-event)))
        (remove nil?))))

(comment
  (let [op +
        main-events [{:value/type :sound :value "bd"
                      :arc/active [0 1/3]}
                     {:value/type :sound :value "cp"
                      :arc/active [1/3 2/3]}
                     {:value/type :sound :value "hh"
                      :arc/active [2/3 1]}]
        step-events [{:value/type :step :value 4
                      :arc/active [0 1/2]}
                     {:value/type :step :value 5
                      :arc/active [1/2 1]}
                     {:value/type :step :value 4
                      :arc/active [1 3/2]}
                     {:value/type :step :value 5
                      :arc/active [3/2 2]}]
        pulse-events [{:value/type :pulse :value 2
                       :arc/active [0 1/3]}
                      {:value/type :pulse :value 3
                       :arc/active [1/3 2/3]}
                      {:value/type :pulse :value 5
                       :arc/active [2/3 1]}
                      {:value/type :pulse :value 2
                       :arc/active [1 4/3]}
                      {:value/type :pulse :value 3
                       :arc/active [4/3 5/3]}
                      {:value/type :pulse :value 5
                       :arc/active [5/3 2]}]
        rotation-events [{:value/type :rotation :value 0
                          :arc/active [0 1]}
                         {:value/type :rotation :value 0
                          :arc/active [1 2]}]
        param-events (reduce (fn [apped-events new-events]
                               (apply-pat-to-pat-both op apped-events new-events))
                             [step-events pulse-events rotation-events])]
    param-events
    #_(merge-pat-to-pat-left main-events param-events)))

(defn apply-pat-to-pat-left
  [op main-events op-events]
  (merge-pat-to-pat-left (partial apply-op-event op) main-events op-events))

(comment

  (apply-op-event +
                  {:value/type :note :value 1}
                  {:value/type :note :value 2})
  (let [op +
        main-events [{:value/type :sound :value "bd"
                      :arc/active [0 1/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [1/3 2/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [2/3 1] :note 1}]
        op-events [{:value/type :note :value 4
                    :arc/active [0 1/6]}
                   {:value/type :note :value 4
                    :arc/active [1/6 1/3]}
                   {:value/type :note :value 5
                    :arc/active [1/3 1]}]]
    (apply-pat-to-pat-left op main-events op-events)))
