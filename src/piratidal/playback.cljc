(ns piratidal.playback
  (:require
   [overtone.at-at :refer [now]]
   [overtone.music.time :refer [apply-at]]
   [piratidal.parser :refer [parse-pattern]]
   [piratidal.pattern :refer [query]]
   [piratidal.api :refer :all]
   [piratidal.superdirt :as sd]
   [time-time.dynacan.players.gen-poly :as gp]))

(defonce cps (atom 90/60)) ; 120 bpm
(defonce current-cycle (atom 0))

(defn cps->bpm [cps]
  (* 60 cps))

(defonce d-patterns
  (atom {}))

(comment
  (-> @d-patterns))

(defonce cycle-dur-ms
  (memoize (fn [cps] (* 1000 (/ 1 cps)))))

(comment
  (reset! cps 9/6)
  (cycle-dur-ms @cps))

(defn schedule-value
  [cycle-start-time {:keys [arc/active] :as event}]
  ;; TODO time is specified as a interval from start-time
  (println "T" (* (- (first active) (int (first active))) (cycle-dur-ms @cps)))
  (apply-at
   (+ cycle-start-time (* (- (first active) (int (first active))) (cycle-dur-ms @cps)))
   #(sd/send-message @sd/osc-client event)))

(comment
  (schedule-value (now) {:value/type :sound
                         :value "bd"
                         :arc/whole [0 1/3]
                         :arc/active [0 1/3]
                         :n 3})
  (schedule-cycle (now) [{:value/type :sound
                          :value "bd"
                          :arc/whole [0 1/2]
                          :arc/active [1 3/2]
                          :n 3}
                         {:value/type :sound
                          :value "bd"
                          :arc/whole [0 1/2]
                          :arc/active [3/2 2]
                          :n 3}]))

#_(cycle-dur-ms @cps)

(defn schedule-cycle
  [cycle-start-time events]
  (doseq [value events]
    #_(println value)
    (schedule-value cycle-start-time value)))

(defonce next-cycle-patterns (atom []))

;;  TODO allow querying at smaller intervals
(defn start-tick
  "starts or updates the cycle"
  [new-cps]
  (reset! cps new-cps)
  (sd/init)
  (let [dur** 1]
    (gp/ref-rain
     :id ::cycle-tick
     :durs [dur**]
     :tempo (cps->bpm new-cps)
     :on-event (gp/on-event
                (schedule-cycle (+ (now) (* dur (cycle-dur-ms @cps)))
                                @next-cycle-patterns)
                (println [@current-cycle (+ dur** @current-cycle)])
                (reset! next-cycle-patterns
                        (mapcat (fn [[_k pattern]]
                                  (query pattern
                                         [@current-cycle (+ dur** @current-cycle)]))
                                @d-patterns))
                (swap! current-cycle + dur**)))))

(comment
  (start-tick 1)
  (gp/stop))

(comment
  (-> @d-patterns)
  (reset! d-patterns {})
  (reset! current-cycle 0)
  (declare d)
  ;; TODO patternize inputs for most things
  (d 1 (sound "[bd bd, hh(3, 8)]")
     (gain "1 <0.5 0.9>")
     (note "2 10 0"))
  (d 2 (sound "~")
     (gain "1 <0.5 0.9> 1")
     (note "<<1 11> {7 9 10} 2> <1 2 8> <3 8 7>")))

(defmacro d
  [id pattern & patterns]
  `(let [pattern# (-> ~pattern ~@patterns)]
     (swap! d-patterns assoc ~id pattern#)
     pattern#))

(comment
  (partition 2 2 (range 0 10 1/2))
  (macroexpand-1
   '(d 1 "a b c"
       (gain "1 <2 1> 3"))))
