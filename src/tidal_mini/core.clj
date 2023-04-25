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
      :fast (fn [x [_ times]]
              [{:stack [(repeat times x)]}])
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
     parse-tree)))

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
                                (= x :silence) {:event :silence
                                                :arc [elapsed-arc end-arc]}
                                (:word x) {:event x
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

                                (or (:alt x) (:polymeter x))
                                (make-schedule*
                                 {:index index
                                  :elapsed-arc elapsed-arc
                                  :ratio (/ ratio length)
                                  :cycle cycle
                                  :slow-cat? true
                                  :polymeter-steps (:steps x)}
                                 (or (:alt x) [(:polymeter x)]))
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
  (->> "bd@2 bd"
       parse-tidal
       transform-tree
       (make-schedule {:index 0 :elapsed-arc 0 :cycle 0})
       flatten))
