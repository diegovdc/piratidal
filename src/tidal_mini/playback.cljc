(ns tidal-mini.playback
  (:require
   [overtone.at-at :refer [now]]
   [overtone.music.time :refer [apply-at stop-player]]
   [tidal-mini.control-patterns :refer [gain note]]
   [tidal-mini.parser :refer [parse-pattern]]
   [tidal-mini.schedule :refer [make-schedule]]
   [time-time.dynacan.players.gen-poly :as gp]
   [tidal-mini.superdirt :as sd]))

(defonce cps (atom 90/60)) ; 120 bpm
(defonce current-cycle (atom 0))

(defn cps->bpm [cps]
  (* 60 cps))

(defonce d-patterns
  (atom {}))

(comment
  (-> @d-patterns))
(do
  (def cycle-dur-ms
    (memoize (fn [cps] (* 1000 (/ 1 cps))))))
(comment
  (reset! cps 9/6)
  (cycle-dur-ms @cps))

(defn schedule-event
  [cycle-start-time {:keys [arc] :as event}]
  (apply-at
   (+ cycle-start-time (* (first arc) (cycle-dur-ms @cps)))
   #(sd/send-message @sd/osc-client event)))

(comment
  (schedule-event (now) {:event {:word "bd"} :arc [1/2 3/4] :gain 1} #_{:event {:word "bd"}, :arc [0 1/3], :cycle 0, :gain 1, :note 12}))

(defn schedule-cycle
  [cycle-start-time events]
  (doseq [event events]
    (schedule-event cycle-start-time event)))

(def next-cycle-patterns (atom []))

(defn start-tick
  "starts or updates the cycle"
  [new-cps]
  (reset! cps new-cps)
  (sd/init)
  (gp/ref-rain
   :id ::cycle-tick
   :durs [7/8 1/8]
   :tempo (cps->bpm new-cps)
   :on-event (gp/on-event
              (cond
                (= 7/8 dur) ;; `dur` is provided by the `on-event` macro
                (do
                    ;; FIXME the usage of current-cycle is kind of funky, semantically speaking
                  (reset! next-cycle-patterns
                          (mapcat (fn [[_k pattern]]
                                    (make-schedule
                                     {:index 0 :elapsed-arc 0 :cycle @current-cycle}
                                     pattern))
                                  @d-patterns))
                  (swap! current-cycle inc))
                (= 1/8 dur)
                (schedule-cycle (+ (now) (* dur (cycle-dur-ms @cps)))
                                @next-cycle-patterns)
                :else (println "Unknown duration")))))

(comment
  (start-tick 1)
  (gp/stop))

(comment
  (declare d)
  (d 1 "[bd <hh*2 sd>]*2"
     (gain "1 <0.5 0.9> 1")
     (note "<1 2> <1 2>"))
  (d 2 "arpy(3,8)"
     (gain "1 <0.5 0.9> 1")
     (note "<1 2> <1 2>")))

(defmacro d
  [id pattern & patterns]
  `(let [pattern# (-> (parse-pattern ~pattern)
                      ~@patterns)]
     (swap! d-patterns assoc ~id pattern#)
     pattern#))

(comment
  (macroexpand-1
   '(d 1 "a b c"
       (gain "1 <2 1> 3"))))
