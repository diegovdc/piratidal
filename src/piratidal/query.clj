(ns piratidal.query
  ;; DEPRECATED only a proof of concept
  (:require
   [clojure.math :as math]
   [piratidal.control-patterns :refer [apply-ctl-pattern]]
   [piratidal.parser :refer [parse-pattern]]
   [piratidal.polymeter :refer [polymeter->stack]]
   [piratidal.utils :refer [wrap-at]]))

(defn pattern-length
  [pattern]
  (reduce (fn [acc el]
            (if (:elongated el)
              (+ acc (:size el))
              (inc acc)))
          0
          pattern))




;; TODO WIP, arc transposition and slow segment  think it's working, needs cleanup
;;


(defn extend-arc
  [speed value]
  (update value :arc (partial map #(* speed %))))

(defn translate-arc
  [arc-span reference-arc value-arc]
  (let [[start end] reference-arc]
    (loop [[ev-start ev-end] value-arc]
      (cond
        (and (<= start ev-start)
             (< ev-start end)) [ev-start ev-end]
        (<= end ev-start) (recur [(- ev-start arc-span)
                                  (- ev-end arc-span)])
        (< ev-start start) (recur [(+ ev-start arc-span)
                                   (+ ev-end arc-span)])))))

(defn take-slow-segment
  [{:keys [speed cycle elapsed-arc end-arc]}
   events]
  (let [ratio (- end-arc elapsed-arc)
        expanded-events (map (partial extend-arc speed) events)
        spans (partition 2 1 (range (* speed elapsed-arc) (+ ratio (* speed end-arc)) ratio))
        [start end] (wrap-at cycle spans)]
    (->> expanded-events
         (reduce
          (fn [acc {:keys [arc] :as event}]
            (let [[ev-start] arc]
              (if-not (and (<= start ev-start)
                           (< ev-start end))
                acc
                (conj acc (assoc event
                                 :arc (translate-arc ratio [elapsed-arc end-arc] arc)
                                  ;; set the actual cycle and not the cycle it was used to calculate the slowed pattern
                                 :cycle cycle)))))
          []))))

(defn normalize-event-time
  [{:as event :keys [arc]}
   & {:keys [scaling-factor query-start]
      :or {scaling-factor 1
           query-start 0}}]
  (let [cycle* (quot (* scaling-factor (first arc)) 1)
        arc* (mapv #(- (* scaling-factor %) cycle*) arc)
        partial? (< (first arc) query-start (second arc))]
    #_(println {:query-start query-start :arc (first arc) :partial? partial?})
    (cond-> (assoc event
                   :cycle cycle*
                   :arc arc*)
      partial?  (assoc :partial? partial?))))
(do
  (defn query*
    [{:keys [index elapsed-arc ratio cycle slow-cat? polymeter-steps]
      :or {index 0
           elapsed-arc 0
           ratio 1
           cycle 0
           slow-cat? false}}
     pattern]
    (let [length (case pattern
                   :stack (pattern-length (:stack pattern))
                   :slowcat 1
                   :polymeter 1
                   (pattern-length pattern))]
      (:events (reduce
                (fn [{:as acc :keys [index elapsed-arc]} x]
                  (let [end-arc (+ elapsed-arc (* (/ 1 length) ratio
                                        ; if `:elongated`
                                                  (:size x 1)))
                        value (cond
                                (or  (:word x)
                                     (int? x)
                                     (double? x)
                                     (ratio? x)
                                     (= x :silence)) {:value x
                                                      :arc [elapsed-arc end-arc]
                                                      :cycle cycle}

                                (:elongated x)
                                (query*
                                 {:index 0
                                  :elapsed-arc elapsed-arc
                                  :ratio (* (/ ratio length)
                                            (:size x))
                                  :cycle cycle}
                                 [(:elongated x)])

                                (vector? x) (query*
                                             {:index index
                                              :elapsed-arc elapsed-arc
                                              :ratio (/ ratio length)
                                              :cycle cycle
                                              :slow-cat? slow-cat?}
                                             x)

                                (:fast x)
                                (->>
                                 (let [speed (:speed x)
                                       span (let [c (* cycle speed)] [c (+ cycle speed)])
                                       cycle-end (mod (last span) 1)
                                       last-cycle (/ (quot (last span) 1)
                                                     speed)
                                       norm-time (fn [events]
                                                   (map #(normalize-event-time
                                                          %
                                                          :scaling-factor (/ 1 speed)
                                                          :query-start (first span))
                                                        events))]
                                   (loop [current-cycle (int (math/floor (first span)))
                                          acc-events []]
                                     (let [events
                                           (norm-time (flatten
                                                       (query*
                                                        {:index 0
                                                         :elapsed-arc (+ elapsed-arc
                                                                         (* current-cycle
                                                                            (/ ratio length)))
                                                         :ratio (/ ratio length)
                                                         :cycle current-cycle}
                                                        [(:fast x)])))
                                           end-time (-> events last :arc second)
                                           cycle* (-> events last :cycle)
                                           stop? (and (>= cycle* last-cycle)
                                                      (> end-time cycle-end))
                                           events* (if-not stop?
                                                     events
                                                     (take-while #(or  (<= (-> % :arc second)
                                                                           cycle-end)
                                                                       (= cycle*
                                                                          (if (= cycle-end 0)
                                                                            (dec last-cycle)
                                                                            last-cycle)))
                                                                 events))
                                           acc-events* (concat acc-events events*)]

                                       (println acc-events)
                                       (when (> (count acc-events*) 2000)
                                         (throw (ex-info "Possible infinite loop on `:fast`" {:data x})))

                                       (if stop?
                                         (->> acc-events*
                                              (drop-while (fn [%]
                                                            #_(println cycle (:cycle %))
                                                            (and (< (:cycle %) cycle)
                                                                 (not (:partial? %)))))
                                              (take-while #(or (< (first (:arc %)) end-arc)
                                                               (:partial? %))))
                                         (recur (inc current-cycle) acc-events*)))))

                                 #_(map
                                    #(query*
                                      {:index 0
                                       :elapsed-arc (+ elapsed-arc
                                                       (* % (/ ratio length (:speed x))))
                                       :ratio (/ ratio length (:speed x))
                                       :cycle (+ (* (:speed x) cycle) %)}
                                      [(:fast x)])
                                    (range (:speed x)))
                                 #_flatten
                                 #_(map #(assoc % :cycle cycle)))

                                (:slow x)
                                (let [ratio* (/ ratio length)]
                                  (->> (query*
                                        {:index 0
                                         :elapsed-arc elapsed-arc
                                         :ratio ratio*
                                         :cycle (quot cycle (:speed x))}
                                        [(:slow x)])
                                       flatten
                                       (take-slow-segment
                                        {:speed (:speed x)
                                         :cycle cycle
                                         :elapsed-arc elapsed-arc
                                         :end-arc end-arc})))

                                (and slow-cat? (:stack x))
                                (->> x
                                     :stack
                                     (map (fn [pat]
                                            (let [cycle* (if polymeter-steps
                                                           (mod cycle polymeter-steps)
                                                           cycle)]
                                              [(wrap-at cycle* pat)])))
                                     (map #(query*
                                            {:index 0
                                             :elapsed-arc elapsed-arc
                                             :ratio (/ ratio length)
                                             :cycle cycle}
                                            %)))

                                (:stack x)
                                (map
                                 #(query*
                                   {:index 0
                                    :elapsed-arc elapsed-arc
                                    :ratio (/ ratio length)
                                    :cycle cycle}
                                   %)
                                 (:stack x))

                                (:slowcat x)
                                (query*
                                 {:index index
                                  :elapsed-arc elapsed-arc
                                  :ratio (/ ratio length)
                                  :cycle (:cycle x cycle)
                                  :slow-cat? true}
                                 (:slowcat x))

                                (:polymeter x)
                                (query*
                                 {:index index
                                  :elapsed-arc elapsed-arc
                                  :ratio (/ ratio length)
                                  :cycle cycle}
                                 [(polymeter->stack cycle x)])

                                (:ctl-type x)
                                (apply-ctl-pattern
                                 (partial query*
                                          {:index index
                                           :elapsed-arc elapsed-arc
                                           :ratio (/ ratio length)
                                           :cycle cycle})
                                 x)

                                :else (println "Warning, unknown pattern: " x))]
                    (if-not value
                      acc
                      (-> acc
                          (update :index inc)
                          (update :events conj value)
                          (assoc :elapsed-arc end-arc
                                 :slow-cat? false)))))
                {:index index :elapsed-arc elapsed-arc :events []}
                pattern))))

  (defn query
    [config pattern]
    (flatten (query* config pattern)))
  #_(->> "<bd sn>"
         parse-tidal
         transform-tree
         (query {:index 0 :elapsed-arc 0})
         flatten)

  (defn pat->query2
    ;; NOTE `cycles` is a list of int
    [pattern cycles]
    (into [] (mapcat (fn [cycle]
                       (->> pattern
                            (query {:index 0 :elapsed-arc 0 :cycle cycle})))
                     cycles)))
  [(pat->query2 (parse-pattern "<hh cp>*3 bd") [1])
   "================="
   #_(-> [{:fast {:word "bd"}, :speed 2}]
         (pat->query2 [0]))
   "================="
   #_(-> [{:fast {:word "bd"}, :speed 2/3}]
         (pat->query2 [1]))])
