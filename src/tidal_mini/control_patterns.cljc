(ns tidal-mini.control-patterns
  (:require [helins.interval.map :as imap]
            [tidal-mini.parser :refer [parse-pattern]]))

(comment
  ;; interval-usage
  (def music
    (-> imap/empty
        ;; Broken C minor chord.
        (imap/mark  0  8 :c)
        (imap/mark  3  8 :e-flat)
        (imap/mark  5  8 :g)
        ;; After a pause, G is repeated.
        (imap/mark 10 11 :g)))
  (music 5)

  (c 4))

(defn ctl-events->imap
  [events]
  (reduce
   (fn [imap {:keys [event arc]}]
     (imap/mark imap
                (first arc)
                (second arc)
                event))
   imap/empty
   events))

(defn simple-ctl
  [ctl-fn]
  (fn [pattern ctl-pattern-str]
    [{:ctl-type :simple
      :ctl-fn ctl-fn
      :ctl-pattern-str ctl-pattern-str
      :pattern pattern}]))

(defn simple-ctl-fn
  [k ctl-events events]
  (let [imap (ctl-events->imap ctl-events)]
    (map (fn [{:keys [arc] :as ev}]
           (assoc ev k (first (imap (first arc)))))
         events)))

(comment
  (def gain-imap
    (ctl-events->imap [{:event 1, :arc [0 1/3]}
                       {:event 0.2, :arc [1/3 2/3]}
                       {:event 3, :arc [2/3 1N]}]))
  (def gain-imap
    (ctl-events->imap [{:event 1, :arc [0 1/3]}
                       {:event 0.2, :arc [1/3 2/3]}
                       {:event 3, :arc [2/3 1N]}]))
  (-> gain-imap)

  (gain-imap 1/3)
  (gain-ctl
   gain-imap
   [{:event {:word "a"}, :arc [0 1/3]}
    {:event {:word "b"}, :arc [1/3 2/3]}
    {:event {:word "c"}, :arc [2/3 1N]}]))

(def gain (simple-ctl (partial simple-ctl-fn :gain)))

(def note (simple-ctl (partial simple-ctl-fn :note)))

(defn apply-ctl-pattern
  [schedule-cycle-fn {:keys [ctl-type ctl-pattern-str ctl-fn pattern] :as ctl-data}]
  (case ctl-type
    :simple (let [events (flatten (schedule-cycle-fn pattern))
                  ctl-events (flatten (schedule-cycle-fn (parse-pattern ctl-pattern-str)))]
              (ctl-fn ctl-events events))))

(gain
 [{:event 1, :arc [0 1/3]}
  {:event 0.2, :arc [1/3 2/3]}
  {:event 3, :arc [2/3 1N]}]
 [{:word "a"} :silence {:word "b"}])

(comment
  (require '[tidal-mini.parser :refer [parse-pattern]])
  (parse-pattern "1 <2 1> 3"))
