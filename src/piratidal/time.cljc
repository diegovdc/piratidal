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

(defn apply-op-to-events
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
             (-> (apply-op-to-events op main-event op-event)
                 (assoc :arc/active arc-intersection)))))
       (remove nil?)))

(defn event->param-map
  "Remove :value, :value/type, :arc/active and :arc/whole  and set value as a simple param of the event"
  [event]
  (-> event
      (dissoc :value :value/type :arc/active :arc/whole)
      (assoc (:value/type event) (:value event))))

(defn merge-params
  [main-event op-event]
  (let [params (event->param-map op-event)
        value ((:value/type main-event) params)]
    (cond-> (merge main-event params)
      value (assoc :value value))))

(defn merge-pat-to-pat-left
  ([main-events op-events] (merge-pat-to-pat-left merge-params main-events op-events))
  ([merge-fn main-events op-events]
   (->> (for [{[start _end] :arc/active :as main-event} main-events
              {[start* end*] :arc/active :as op-event} op-events]
          (when (and (>= start start*)
                     (< start end*))
            (merge-fn main-event op-event)))
        (remove nil?))))

(defn apply-pat-to-pat-left
  [op main-events op-events]
  (merge-pat-to-pat-left (partial apply-op-to-events op) main-events op-events))
