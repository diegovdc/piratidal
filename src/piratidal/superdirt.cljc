(ns piratidal.superdirt
  (:require [overtone.osc :as osc]
            [piratidal.pattern :refer [silence?]]))

(defonce osc-client (atom nil))

(defn init []
  (when-not @osc-client
    (reset! osc-client (osc/osc-client "0.0.0.0" 57120))
    @osc-client))

(defonce _id_ (atom -1))

(do

  (defn value->super-dirt-args
    [value]
    (into {} (map (fn [[k v]]
                    (cond
                      (string? v) [(name k) v]
                      (int? v) [(name k) (int v)]
                      :else [(name k) (float v)]))
                  (-> value
                      (dissoc :value :arc/active :arc/whole :pattern/type :value/type)
                      (assoc (:value/type value) (:value value))))))

  (value->super-dirt-args
   {:value/type :sound
    :value "bd"
    :arc/whole [0 1/3]
    :arc/active [0 1/3]
    :n 3}))

(defn make-play-msg
  [value]
  (-> {"_id_", (int (swap! _id_ inc))
       "cps", (float 0.5625), ;; TODO unhardcode
       "orbit", (int 0)}
      (merge (value->super-dirt-args value))
      seq
      (conj "/dirt/play")
      flatten))

(comment
  (make-play-msg
   {:value/type :sound
    :value "bd"
    :arc/whole [0 1/3]
    :arc/active [0 1/3]
    :n 3}))

(defn send-message*
  [osc-send osc-client value]
  (when-not (silence? value)
    (apply osc-send osc-client (make-play-msg value))))

(def send-message
  (partial send-message* osc/osc-send))

(comment
  (send-message @osc-client
                {:value/type :sound
                 :value "bd"
                 :arc/whole [0 1/3]
                 :arc/active [0 1/3]
                 :n 3}))
(comment
  ;; example
  (osc/osc-debug true)
  (init)
  (osc/osc-send @osc-client "/dirt/play", "_id_", (int 1), "cps", (float 0.5625), "cycle", (float 28294.0), "delta", (float 3.5555520057678)
                "orbit", (int 0), "s", "bd")
  (apply osc/osc-send @osc-client
         (make-play-msg {:value/type :sound
                         :value "bd"
                         :arc/whole [0 1/3]
                         :arc/active [0 1/3]
                         :n 3})))
