(ns tidal-mini.core
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [tidal-mini.euclidean-rhythm :refer [euclidean-rhythm]]
   [tidal-mini.utils :refer [rotate wrap-at]]))

(do
  (def tidal-pattern-grammar
    (slurp "src/tidal_mini/tidal.grammar"))

  (defn parse-tidal
    [input & {:keys [check-ambiguous?]
              :or {check-ambiguous? false}}]
    (let [parser (insta/parser tidal-pattern-grammar)
          parsed-data (insta/parse parser input)]
      (when check-ambiguous?
        (let [parses (insta/parses parser input)]
          (when (> (count (seq parses)) 1)
            (throw (ex-info "Grammar is ambiguous"
                            {:parses parses})))))
      (if (insta/failure? parsed-data)
        (println (insta/get-failure parsed-data))
        parsed-data)))

  (let [input "a ~  b*2 [c*2 c] <d e> . <a b>!2 a/2 [a , <b , c>]"]
    (parse-tidal input))
  (let [input "bd [bd <sn hh>]"]
    (parse-tidal input))
  (let [input "bd {bd hh sn}"]
    (parse-tidal input))
  (let [input "bd {bd hh sn}%5"]
    (parse-tidal input))
  (let [input "bd [bd | sn]"]
    (parse-tidal input))
  (let [input "bd@2 bd"]
    (parse-tidal input))
  (let [input "bd:2 bd"]
    (parse-tidal input))
  (let [input "bd(3, 8, 1)"]
    (parse-tidal input))
  (let [input "bd bd?0.5"]
    (parse-tidal input))
  (let [input "bd {bd hh sn, hh sn}"]
    (parse-tidal input))
  #_(let [input "bd@ bd?0.5"]
      (parse-tidal-pattern input)))

(defn polymeter-step-at-cycle&index
  "Calculate the number of times a current step has been seen in the polymeter. For debugging purposes will return a map wit the event-index and the times-seen

  The following tables are examples of the calculation
  For example in a {1 2}%3 polymeter:
  0     1     2       ; cycle
  0 1 2 0 1 2 0 1 2   ; current-step-index
  1 2 1 2 1 2 1 2 1   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; event-index
  0 0 1 1 2 2 3 3 4   ; times-seen

  And a {1 2 3}%4 polymeter:
  0       1       2           ; cycle
  0 1 2 3 0 1 2 3 0 1 2  3    ; current-step-index
  1 2 3 1 2 3 1 2 3 1 2  3    ; current-polymeter-step
  0 1 2 3 4 5 6 7 8 9 10 11   ; event-index
  0 0 0 1 1 1 2 2 2 3 3  3    ; times-seen

  And a {1 2 3}%2 polymeter:
  0   1   2   3   4   ; cycle
  0 1 0 1 0 1 0 1 0   ; current-step-index
  1 2 3 1 2 3 1 2 3   ; current-polymeter-step
  0 1 2 3 4 5 6 7 8   ; event-index
  0 0 0 1 1 1 2 2 2   ; times-seen"
  [polymeter-size polymeter-steps cycle current-step-index]
  (let [event-index (+ (* cycle polymeter-steps) current-step-index)]
    {:event-index event-index
     :times-seen (quot event-index polymeter-size)}))

(do

  (defn polymeter->stack
    [cycle {:keys [polymeter steps]}]
    {:stack
     (let [cats (-> polymeter :stack)]
       (map (fn [cat]
              (:cat
               (reduce
                (fn [acc step]
                  (let [pat (wrap-at (+ (* cycle steps) step) cat)
                        alt? (:alt pat)
                        pat* (if-not alt? pat
                                     (assoc pat :cycle (:times-seen
                                                        (polymeter-step-at-cycle&index
                                                         (count cat)
                                                         steps
                                                         cycle
                                                         step))))]
                    (update acc :cat conj pat*)))
                {:cat []
                 :alt-indexes {}}
                (range steps))))
            cats))})
  (polymeter->stack
   1
   {:polymeter
    {:stack
     [[{:word "bd"} {:alt [{:stack [[{:word "hh"} {:word "cp"} 808]]}]}]]}
    :steps 3}))

(do
  (defn transform-tree
    [parse-tree]
    (insta/transform
     {:pattern identity
      :cat (fn [& xs]
             (reduce
              (fn [acc pat]
                (cond
                  (:replicate pat) (into [] (concat acc (:replicate pat)))
                  (:euclidean pat) (into [] (concat acc (:euclidean pat)))
                  :else (conj acc pat)))
              []
              xs))
      :word (fn [x] {:word x})
      :sample (fn [& [{:keys [word]} [_ n]]]
                {:sample word :n n})
      :int (fn [int-str] (int (edn/read-string int-str)))
      :float (fn [float-str]
               (let [float-str* (if (str/starts-with? float-str ".")
                                  (str "0" float-str)
                                  float-str)]
                 (double (edn/read-string float-str*))))
      :silence (constantly :silence)
      :group vector
      :stack (fn [& xs] {:stack (into [] xs)})
      :alt (fn [& xs] {:alt (into [] xs)})
      :fast (fn [x [_ speed]]
              {:fast x
               :speed speed})
      :slow (fn [x [_ speed]]
              {:slow x
               :speed speed})
      :replicate (fn [pat [_ times]]
                   {:replicate (repeat times pat)})
      :polymeter (fn [& [stack [_ steps]]]
                   {:polymeter stack
                    :steps (or steps (->> stack :stack (map count) (apply max)))})
      :degrade (fn [& [stack [_ amount]]]
                 {:degrade stack
                  :amount (or amount 0.5)})
      :elongate (fn [& [pat [_ n]]]
                  {:elongated pat
                   :size n})
      :euclidean (fn [& [pat [_ pulses steps phase]]]
                   {:euclidean (-> (euclidean-rhythm pulses steps)
                                   (rotate (or phase 0))
                                   (->> (map (fn [play?]
                                               (if-not (zero? play?)
                                                 pat :silence)))))})}
     parse-tree))
  (transform-tree
   [:pattern
    [:cat
     [:word "bd"]
     [:polymeter
      [:stack
       [:pattern [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]
       [:pattern [:cat [:word "hh"] [:word "sn"]]]]
      [:steps 5]]]]))

(do
  (defn pattern-length
    [pattern]
    (reduce (fn [acc el]
              (if (:elongated el)
                (+ acc (:size el))
                (inc acc)))
            0
            pattern))
  (pattern-length [{:elongated {:word "bd"}, :size 2} {:word "bd"}]))

(do

  (defn take-slow-segment
    ;; FIXME use `elapsed-arc` and `end-arc`
    ;; TODO  refactor for performance and clarity
    ;; TODO also the logic for this and for the `:fast` case should be the same but the speed value would be inverted
    [cycle ratio speed events]
    (->> events
         (map (fn [event-data]
                (update event-data :arc
                        (fn [arc]
                          (map #(* % speed) arc)))))
         (filter (fn [{:keys [arc]}]
                   (let [min* (* (mod cycle speed) ratio)
                         max* (+ min* ratio)]
                     (and (<= min* (first arc))
                          (< (first arc) max*)))))
         (map (fn [event-data]
                (update event-data :arc
                        (fn [arc]
                          (mapv #(- % (* (mod cycle speed) ratio))
                                arc)))))))

  (take-slow-segment
   3 1/2 3
   [{:event {:word "a"}, :arc [0 1/4]}
    {:event {:word "b"}, :arc [1/4 1/2]}]))


;; TODO WIP, arc transposition and slow segment  think it's working, needs cleanup
;;


(do
  (defn extend-arc
    [speed event]
    (update event :arc (partial map #(* speed %))))

  (extend-arc
   3
   #_{:event {:word "a"}, :arc [0 1/4]}
   {:event {:word "a"}, :arc [1/2 3/4]}))

(do
  #dbg
   (defn translate-arc
     [arc-span reference-arc event-arc]
     (let [[start end] reference-arc]
       (loop [[ev-start ev-end] event-arc]
         (cond
           (and (<= start ev-start)
                (< ev-start end)) [ev-start (min end ev-end)]
           (<= end ev-start) (recur [(- ev-start arc-span)
                                     (- ev-end arc-span)])
           (< ev-start start) (recur [(+ ev-start arc-span)
                                      (+ ev-end arc-span)])))))
  (translate-arc 1/2 [0 1/2] [3/4 1]))

(do
  #dbg
   (defn take-slow-segment2
     [{:keys [speed cycle elapsed-arc end-arc]}
      events]
     (let [ratio (- end-arc elapsed-arc)
           expanded-events (map (partial extend-arc speed) events)
           spans (partition 2 1 (range (* speed elapsed-arc) (+ ratio (* speed end-arc)) ratio))
           [start end] (wrap-at cycle spans)]
       (->> expanded-events
            (reduce
             (fn [acc {:keys [arc] :as event}]
               (let [[ev-start ev-end] arc]
                 (if-not (and (<= start ev-start)
                              (< ev-start end))
                   acc
                   (conj acc (assoc event :arc
                                    (translate-arc ratio [elapsed-arc end-arc] arc))))))
             []))))
  (take-slow-segment2
   {:speed 1
    :cycle 1
    :elapsed-arc 1/2
    :end-arc 1}
   [{:event {:word "a"}
     :arc [1/2 3/4]}
    {:event {:word "b"}
     :arc [3/4 1N]}]))

(do
  #dbg
   (defn make-schedule*
     [{:keys [index elapsed-arc ratio cycle slow-cat? polymeter-steps]
       :or {index 0
            elapsed-arc 0
            ratio 1
            cycle 0
            slow-cat? false}}
      pattern]
     (let [length (case pattern
                    :stack (pattern-length (:stack pattern))
                    :alt 1
                    :polymeter 1
                    (pattern-length pattern))]
       (:events (reduce
                 (fn [{:as acc :keys [index elapsed-arc]} x]
                   (let [end-arc (+ elapsed-arc (* (/ 1 length) ratio
                                        ; if `:elongated`
                                                   (:size x 1)))
                         event (cond
                                 (or  (:word x)
                                      (:int x)
                                      (:float x)
                                      (= x :silence)) {:event x
                                                       :arc [elapsed-arc end-arc]}

                                 (:elongated x)
                                 (make-schedule*
                                  {:index 0
                                   :elapsed-arc elapsed-arc
                                   :ratio (* (/ ratio length)
                                             (:size x))
                                   :cycle cycle}
                                  [(:elongated x)])

                                 (vector? x) (make-schedule*
                                              {:index index
                                               :elapsed-arc elapsed-arc
                                               :ratio (/ ratio length)
                                               :cycle cycle
                                               :slow-cat? slow-cat?}
                                              x)

                                 (:fast x)
                                 (map
                                  #(make-schedule*
                                    {:index 0
                                     :elapsed-arc (+ elapsed-arc
                                                     (* % (/ ratio length (:speed x))))
                                     :ratio (/ ratio length (:speed x))
                                     :cycle (+ (* (:speed x) cycle) %)}
                                    [(:fast x)])
                                  (range (:speed x)))

                                 (:slow x)
                                 (let [ratio* (/ ratio length)]
                                   (->> (make-schedule*
                                         {:index 0
                                          :elapsed-arc elapsed-arc
                                          :ratio ratio*
                                          :cycle (quot cycle (:speed x))}
                                         [(:slow x)])
                                        flatten
                                        #_(take-slow-segment cycle ratio* (:speed x))
                                        (take-slow-segment2
                                         {:speed (:speed x)
                                          :cycle cycle
                                          :elapsed-arc elapsed-arc
                                          :end-arc end-arc})
                                        #_(filter (fn [{:keys [arc] :as ev}]
                                                    (let [range* (/ 1 (* (:speed x) ratio*))
                                                          min* (* (mod cycle (:speed x)) range*)
                                                          max* (+ min* range*)]
                                                      (and (<= min* (first arc))
                                                           (< (first arc) max*)))))
                                        #_(map (fn [event-data]
                                                 (update event-data :arc
                                                         (fn [arc]
                                                           (map #(* % (:speed x))
                                                                arc)))))))

                                 (and slow-cat? (:stack x))
                                 (->> x
                                      :stack
                                      (map (fn [pat]
                                             (let [cycle* (if polymeter-steps
                                                            (mod cycle polymeter-steps)
                                                            cycle)]
                                               [(wrap-at cycle* pat)])))
                                      (map #(make-schedule*
                                             {:index 0
                                              :elapsed-arc elapsed-arc
                                              :ratio (/ ratio length)
                                              :cycle cycle}
                                             %)))

                                 (:stack x)
                                 (map
                                  #(make-schedule*
                                    {:index 0
                                     :elapsed-arc elapsed-arc
                                     :ratio (/ ratio length)
                                     :cycle cycle}
                                    %)
                                  (:stack x))

                                 (:alt x)
                                 (make-schedule*
                                  {:index index
                                   :elapsed-arc elapsed-arc
                                   :ratio (/ ratio length)
                                   :cycle (:cycle x cycle)
                                   :slow-cat? true}
                                  (:alt x))

                                 (:polymeter x)
                                 (make-schedule*
                                  {:index index
                                   :elapsed-arc elapsed-arc
                                   :ratio (/ ratio length)
                                   :cycle cycle}
                                  [(polymeter->stack cycle x)])

                                 :else (println "Warning, unknown pattern: " x))]
                     (if-not event
                       acc
                       (-> acc
                           (update :index inc)
                           (update :events conj event)
                           (assoc :elapsed-arc end-arc
                                  :slow-cat? false)))))
                 {:index index :elapsed-arc elapsed-arc :events []}
                 pattern))))

  (defn make-schedule
    [config pattern]
    (flatten (make-schedule* config pattern)))
  #_(->> "<bd sn>"
         parse-tidal
         transform-tree
         (make-schedule {:index 0 :elapsed-arc 0})
         flatten)
  (->> "c [a <b d>]/3"
       parse-tidal
       transform-tree
       (make-schedule {:index 0 :elapsed-arc 0 :cycle 0})))

[[{:stack
   [[{:polymeter
      {:stack
       [[{:word "bd"} {:alt [{:stack [[{:word "hh"} {:word "cp"} 808]]}]}]]}
      :steps 3}]]}]
 [{:stack
   [[{:polymeter
      {:stack
       [[{:word "bd"} {:alt [{:stack [[{:word "hh"} {:word "cp"} 808]]}]}]]}
      :steps 3}]]}]]
