(ns tidal-mini.control-patterns
  (:require
   [clojure.core :as math]
   [helins.interval.map :as imap]
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
   (fn [imap {:keys [value arc]}]
     (imap/mark imap
                (first arc)
                (second arc)
                value))
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
    (ctl-events->imap [{:value 1, :arc [0 1/3]}
                       {:value 0.2, :arc [1/3 2/3]}
                       {:value 3, :arc [2/3 1N]}]))
  (def gain-imap
    (ctl-events->imap [{:value 1, :arc [0 1/3]}
                       {:value 0.2, :arc [1/3 2/3]}
                       {:value 3, :arc [2/3 1N]}]))
  (-> gain-imap)

  (gain-imap 1/3)
  (gain-ctl
   gain-imap
   [{:value {:word "a"}, :arc [0 1/3]}
    {:value {:word "b"}, :arc [1/3 2/3]}
    {:value {:word "c"}, :arc [2/3 1N]}]))

(def gain (simple-ctl (partial simple-ctl-fn :gain)))

(def note (simple-ctl (partial simple-ctl-fn :note)))

(do
  #_(defn rev-ctl
      [events & {:keys [elapsed-arc] :or {elapsed-arc 0}}]
      (let [rev-events (reverse events)]
        (:events
         (reduce
          (fn [{:keys [elapsed-arc cycle] :as acc} {:keys [arc] :as ev}]
            (let [next-cycle? (>= elapsed-arc 1)
                  elapsed-arc* (if next-cycle? (- elapsed-arc 1) elapsed-arc)
                  arc-span (- (second arc) (first arc))
                  arc-end (+ elapsed-arc* arc-span)
                  cycle* (if next-cycle? (inc cycle) cycle)]
              (-> acc
                  (update :events conj
                          (-> ev
                              (assoc :arc [elapsed-arc* arc-end])
                              (assoc :cycle cycle*)))
                  (assoc :elapsed-arc arc-end))))
          {:events [] :elapsed-arc elapsed-arc :cycle (-> events first :cycle)}
          rev-events))))
  (defn rev-ctl
    [events & {:keys [elapsed-arc] :or {elapsed-arc 0}}]
    (let [events* (partition-by :cycle events)]
      (->> events*
           (mapcat
            (fn [events]
              (:events
               (reduce
                (fn [{:keys [elapsed-arc cycle] :as acc}
                     {:keys [arc] :as ev}]
                  (let [next-cycle? (>= elapsed-arc 1)
                        ends-on-next-cycle? (> (second arc) 1)
                        elapsed-arc* (if next-cycle? (- elapsed-arc 1) elapsed-arc)
                        arc-span (- (second arc) (first arc))
                        arc-end (+ elapsed-arc* arc-span)
                        cycle* (cond
                                 ;; next-cycle? (inc cycle)
                                 ;; ends-on-next-cycle? (dec cycle)
                                 :else cycle)]
                    (-> acc
                        (update :events conj
                                (-> ev
                                    (assoc :arc [(- 1 (second arc))
                                                 (- 1 (first arc))] #_[elapsed-arc* arc-end])
                                    (assoc :cycle cycle*)))
                        (assoc :elapsed-arc arc-end))))
                {:events [] :elapsed-arc elapsed-arc :cycle (-> events first :cycle)}
                (reverse events))))))))

  (rev-ctl
   [{:value 1 :arc [0 2] :cycle 0}])
  (rev-ctl
   [{:value {:word "bd"}, :arc [0 2/3], :cycle 0}
    {:value :silence, :arc [2/3 4/3], :cycle 0}])
  #_(defn reflect [cycle [arc-b arc-e]]
      [(+ cycle (- (inc cycle) arc-e))
       (+ cycle (- (inc cycle) arc-b))])

  #_(reflect 0 [0 2]))

(partition-by :cycle [{:value 1 :arc [0 1/2] :cycle 0}
                      {:value 2 :arc [1/2 1] :cycle 0}
                      {:value 3 :arc [0 1/2] :cycle 1}
                      {:value 4 :arc [1/2 1] :cycle 1}])

(defn rev [pattern]
  [{:ctl-type :rev
    :ctl-fn rev-ctl
    :pattern pattern}])

(defn slow [pattern speed]
  [{:slow pattern
    :speed speed}])

#_(do
    (defn palindrome-fast
      [events]
      (let [speed 2]
        (map
         (fn [ev]
           (update ev :arc (fn [[s e]] [(/ s speed) (/ e speed)])))
         events)))

    (palindrome-fast [{:value {:word "bd"}, :arc [0N 1/3], :cycle 0}
                      {:value {:word "hh"}, :arc [1/3 2/3], :cycle 0}
                      {:value {:word "sn"}, :arc [2/3 1], :cycle 0}])
    (palindrome-fast [{:value {:word "bd"}, :arc [0 2], :cycle 0}]))

(do
  (defn palindrome-ctl
    [events]
    (let [first-cycle (-> events first :cycle)]
      (->> events
           (partition-by :cycle)
           (mapcat (fn [evs]
                     (concat
                      (map (fn [ev]
                             (update ev :cycle
                                     #(->
                                       %
                                       #_(- first-cycle)
                                       (* 2)
                                       #_(+ first-cycle))))
                           evs)
                      (map (fn [ev]
                             (update ev :cycle
                                     #(->
                                       %
                                       #_(- first-cycle)
                                       (* 2)
                                       inc
                                       #_(+ first-cycle))))
                           (rev-ctl evs)))))
           (sort-by :cycle))))

  (palindrome-ctl
   #_[{:value {:word "bd"}, :arc [0 1/3], :cycle 0}
      {:value {:word "hh"}, :arc [1/3 2/3], :cycle 0}
      {:value {:word "sn"}, :arc [2/3 1N], :cycle 0}
      {:value {:word "bd"}, :arc [0 1/3], :cycle 1}
      {:value {:word "hh"}, :arc [1/3 2/3], :cycle 1}
      {:value {:word "cp"}, :arc [2/3 1N], :cycle 1}]
   [{:value {:word "bd"}, :arc [0 1/3], :cycle 3}
    {:value {:word "bd"}, :arc [1/3 2/3], :cycle 3}
    {:value :silence, :arc [2/3 1N], :cycle 3}
    {:value {:word "hh"}, :arc [0N 1/3], :cycle 4}
    {:value {:word "hh"}, :arc [1/3 2/3], :cycle 4}
    {:value :silence, :arc [2/3 1N], :cycle 4}
    {:value {:word "cp"}, :arc [0N 1/3], :cycle 5}
    {:value {:word "cp"}, :arc [1/3 2/3], :cycle 5}
    {:value :silence, :arc [2/3 1N], :cycle 5}])
  (palindrome-ctl
   [{:value {:word "bd"}, :arc [0 2/3], :cycle 0}
    {:value :silence, :arc [2/3 4/3], :cycle 0}
    {:value :silence, :arc [1/3 1N], :cycle 1}
    {:value {:word "bd"}, :arc [0 2/3], :cycle 2}
    {:value :silence, :arc [2/3 4/3], :cycle 2}]))

(comment
  (require '[tidal-mini.parser :as p]
           '[tidal-mini.schedule :as sch])
  (sch/pat->schedule2 (p/parse-pattern "[bd ~ ~]/2")
                      (range 3)))

(defn palindrome [pattern]
  [{:ctl-type :palindrome
    :ctl-fn palindrome-ctl
    :pattern pattern}])

(defn apply-ctl-pattern
  [schedule-cycle-fn {:keys [ctl-type ctl-pattern-str ctl-fn pattern]}]
  (let [make-events #(flatten (schedule-cycle-fn pattern))]
    (case ctl-type
      :simple (let [ctl-events (flatten (schedule-cycle-fn (parse-pattern ctl-pattern-str)))]
                (ctl-fn ctl-events (make-events)))
      :rev (ctl-fn (make-events))
      :palindrome (ctl-fn (make-events)))))

(gain
 [{:value 1, :arc [0 1/3]}
  {:value 0.2, :arc [1/3 2/3]}
  {:value 3, :arc [2/3 1N]}]
 [{:word "a"} :silence {:word "b"}])

(comment
  (require '[tidal-mini.parser :refer [parse-pattern]])
  (parse-pattern "1 <2 1> 3"))
