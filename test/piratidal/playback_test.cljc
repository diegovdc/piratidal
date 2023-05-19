(ns piratidal.playback-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.playback :refer [get-event-onset-time]]))

(deftest get-event-onset-time-test
  (is (= [1875.0 2000.0 2125.0 2250.0 2375.0 2500.0]
         (mapv float
               [(get-event-onset-time 1 1/2 1000 1500 1/2 7/8)
                (get-event-onset-time 1 1/2 1500 2000 1   1)
                (get-event-onset-time 1 1/2 1500 2000 1   9/8)
                (get-event-onset-time 1 1/2 1500 2000 1   10/8)
                (get-event-onset-time 1 1/2 1500 2000 1   11/8)
                (get-event-onset-time 1 1/2 2000 2500 3/2 12/8)])))
  (testing "Supports changes in cps"
    (is (= [1875.0 2000.0 2062.5 2125.0 2187.5 2250.0]
           (mapv float
                 [(get-event-onset-time 1 1/2 1000 1500 1/2 7/8)
                  (get-event-onset-time 2 1/2 1500 2000 1   1)
                  (get-event-onset-time 2 1/2 1500 2000 1   9/8)
                  (get-event-onset-time 2 1/2 1500 2000 1   10/8)
                  (get-event-onset-time 2 1/2 1500 2000 1   11/8)
                  (get-event-onset-time 2 1/2 2000 2250 3/2 12/8)])))))
