(ns tidal-mini.time)

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
  [op op-event main-event]
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
  [op op-events main-events]
  (->> (for [{arc :arc/active :as main-event} main-events
             {arc* :arc/active :as op-event} op-events]
         (when-let [arc-intersection (sub-arc arc arc*)]
           (-> (apply-op-event op op-event main-event)
               (assoc :arc/active arc-intersection))))
       (remove nil?)))

(comment
  (apply-op-event +
                  {:value/type :note :value 1}
                  {:value/type :note :value 2})
  (let [op +
        main-events [{:value/type :note :value 1
                      :arc/active [0 1/3]}
                     {:value/type :note :value 2
                      :arc/active [1/3 2/3]}
                     {:value/type :note :value 3
                      :arc/active [2/3 1]}]
        op-events [{:value/type :note :value 4
                    :arc/active [0 1/2]}
                   {:value/type :note :value 5
                    :arc/active [1/2 1]}]]
    (apply-pat-to-pat-both op op-events main-events)))
