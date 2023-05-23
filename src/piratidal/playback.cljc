
(ns piratidal.playback
  (:require
   [clojure.core.async :as async]
   [overtone.at-at :refer [now]]
   [overtone.music.time :refer [apply-at]]
   [piratidal.pattern :refer [query]]
   [piratidal.superdirt :as sd]
   [piratidal.utils :refer [cps->bpm]]
   [time-time.dynacan.players.gen-poly :as gp]))

(defonce patterns (atom {}))

(defonce ticker-state (atom {:cps 1
                             :previous-tick-onset-ms nil
                             :elapsed-ms nil
                             :cycle 0
                             :tick-cycle-dur 1}))

(defn get-event-onset-time
  ;; TODO probably the logic can be improved, and maybe there are bugs in here
  "A sort of fancy way to calculate the event onset based on the current tick and the previous tick.
  This is done in order to better support dynamic changes in cps as well as tick frequency.
  NOTE: onset times are on milliseconds"
  [cps
   tick-cycle-dur
   previous-tick-onset
   tick-onset
   tick-cycle-point
   event-point]
  (let [tick-ratio (if (= 1 tick-cycle-dur) 1 (/ tick-cycle-dur cps))
        delta-ratio (/ (- event-point tick-cycle-point) tick-ratio)
        delta-time* (- tick-onset previous-tick-onset)
        delta-time (if (zero? delta-time*) 1 delta-time*)]
    (+ tick-onset (* delta-ratio delta-time))))

(defn schedule-events!
  [{:keys [cps tick-cycle-dur previous-tick-onset-ms elapsed-ms cycle] :as _ticker-state-data}
   events]
  (doseq [event events]
    (when-not (false? (:has-start? event))
      (let [{[onset] :arc/active} event
            onset-time (get-event-onset-time cps
                                             tick-cycle-dur
                                             previous-tick-onset-ms
                                             elapsed-ms
                                             cycle
                                             onset)]
        (apply-at onset-time
                  #(sd/send-message @sd/osc-client (assoc event :cycle cycle)))))))

(defn get-tick-dur-ms
  [cps tick-cycle-dur]
  (* (/ 1000 cps) tick-cycle-dur))

(defn on-tick [_]
  (let [{:keys [cps tick-cycle-dur elapsed-ms cycle]
         :as ticker-state-data} @ticker-state
        tick-dur (get-tick-dur-ms cps tick-cycle-dur)
        next-cycle (+ cycle tick-cycle-dur)
        events (doall (mapcat (fn [[_k pattern]] (query pattern [cycle next-cycle]))
                              @patterns))]
    (schedule-events! ticker-state-data events)
    (swap! ticker-state #(-> %
                             (assoc :previous-tick-onset-ms elapsed-ms
                                    :cycle next-cycle)
                             (update :elapsed-ms + tick-dur)))))

(defn start-ticker
  "starts or updates the cycle"
  [new-cps]
  (if-not (:ticker @gp/refrains)
    (let [elapsed-ms (now)
          tick-cycle-dur (:tick-cycle-dur @ticker-state)]
      (swap! ticker-state assoc
             :cps new-cps
             :elapsed-ms elapsed-ms
             :previous-tick-onset-ms (- elapsed-ms (get-tick-dur-ms new-cps tick-cycle-dur))
             :cycle 0))
    (swap! ticker-state assoc :cps new-cps))
  (sd/init)
  (gp/ref-rain
   :id :ticker
   :durs [(:tick-cycle-dur @ticker-state)]
   :tempo (cps->bpm new-cps)
   :on-event #'on-tick)) []

(defn playing? []
  (boolean (:ticker @gp/refrains)))
