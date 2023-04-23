(ns tidal-mini.core
  (:require [instaparse.core :as insta]))

(do
  (def tidal-pattern-grammar
    (slurp "src/tidal_mini/grammar.txt"))

  (defn parse-tidal-pattern [input]
    (let [parser (insta/parser tidal-pattern-grammar)]
      (insta/parse  parser input)))

  (let [input "a ~  b*2 [c*2 c] <d e> . <a b>!2 a/2 [a , <b , c>]"]
    (parse-tidal-pattern input))
  (let [input "bd [bd <sn hh>]"]
    (parse-tidal-pattern input))
  (let [input "bd {bd hh sn}"]
    (parse-tidal-pattern input))
  (let [input "bd {bd hh sn}%5"]
    (parse-tidal-pattern input)))

(do
                                        ;#dbg
  (defn make-schedule [{:keys [index elapsed-arc ratio]
                        :or {index 0
                             elapsed-arc 0
                             ratio 1}}
                       pattern]
    (let [length (case pattern
                   :stack (count (:stack pattern))
                   :alt 1               ;TODO maybe fixme(count (:alt pattern))
                   (count pattern))]
      (:events (reduce
                (fn [{:as acc :keys [index elapsed-arc]} x]
                  #_(when (:word x)
                      (println (:word x)))
                  (let [end-arc (+ elapsed-arc (* (/ 1 length) ratio))
                        event (cond
                                (:word x) {:event x :arc [elapsed-arc end-arc]}
                                (vector? x) (make-schedule
                                             {:index index
                                              :elapsed-arc elapsed-arc
                                              :ratio (/ ratio length)} x)
                                (:stack x) (map
                                            #(make-schedule
                                              {:index 0
                                               :elapsed-arc elapsed-arc
                                               :ratio (/ ratio length)}
                                              %)
                                            (:stack x))
                                (:alt x) (map
                                          #(make-schedule
                                            {:index index
                                             :elapsed-arc elapsed-arc
                                        ;TODO hardcoded FIXME
                                             :ratio (* 2 ratio)}
                                            %)
                                          (:alt x))
                                :else nil)]
                    (if-not event
                      acc
                      (-> acc
                          (update :index inc)
                          (update :events conj event)
                          (assoc :elapsed-arc end-arc)))))
                {:index index :elapsed-arc elapsed-arc :events []}
                pattern))))

  (->> "bd [bd [sn , [hh bd]]]"
       parse-tidal-pattern
       (insta/transform
        {:cat vector
         :word (fn [x] {:word x})
         :pattern identity
         :group vector
         :stack (fn [& xs] {:stack (into [] xs)})
         :alt (fn [& xs] {:alt (into [] xs)})})
       (make-schedule {:index 0 :elapsed-arc 0})
       flatten))
