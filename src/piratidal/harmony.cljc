(ns piratidal.harmony
  (:require
   [piratidal.utils :refer [wrap-nth]]))

(defonce scales
  ;; "A map of scale name (string) to {:ratios [] :period number}"
  (atom {}))

#_:clj-kondo/ignore
(defn set-scales!
  [new-scales]
  (swap! scales merge new-scales))

(comment
  (set-scales!
   {"hex1" {:ratios [35/32 5/4 21/16 3/2 7/4 15/8]
            :period 2}
    "hex2" {:ratios [1 1 9/7 5/3 15/7 15/7]
            :period 3}}))

(defn- get-period [scale-len degree]
  (let [transp-period* (if (> 0 degree) -1 0)
        ;; prevent the zeroth degree to be counted as the start of lower octave
        ;; else our tranposition algorithm will overtranspose this degree
        degree* (if (> 0 degree) (inc degree) degree)]
    (+ transp-period* (quot degree* scale-len))))

(defn- ratio->cents [ratio]
  (-> (Math/log ratio) (/ (Math/log 2)) (* 1200)))

(defn- ratio->midi
  [ratio]
  (/ (ratio->cents ratio)
     100))

(defn- get-transp-fn [scale-period]
  (if (>= scale-period 0) * /))

(defn- transpose-by [bounding-period scale-period]
  (apply (get-transp-fn scale-period)
         1 ;; important when transp-fn is division
         (repeat (Math/abs (double scale-period)) bounding-period)))

(defn- scaled-note
  [{:keys [ratios period]} note]
  (let [ratio (wrap-nth ratios note)
        period* (get-period (count ratios) note)]
    (ratio->midi (* ratio (transpose-by period period*)))))
(comment
  (scaled-note (get @scales "hex1") 6))

(defn apply-scale2
  [{:keys [note scale2] :as ev}]
  (if (and note scale2)
    (-> ev
        (assoc :note (scaled-note
                      (get @scales scale2 {:ratios [1] :period 2})
                       ;; notes are degrees (indexes) of a scale so they should be integers
                      (int note)))
        (dissoc :scale2))
    ev))
(comment
  (apply-scale2 {:s "bd" :note 6 :scale2 "hex2"}))
