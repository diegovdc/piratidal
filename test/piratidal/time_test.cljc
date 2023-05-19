(ns piratidal.time-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.time
    :refer
    [apply-op-to-events
     apply-pat-to-pat-both
     apply-pat-to-pat-left
     event->param-map
     merge-pat-to-pat-left]]))

(deftest apply-op-to-events-test
  (is (= {:value/type :note, :value 3}
         (apply-op-to-events +
                             {:value/type :note :value 1}
                             {:value/type :note :value 2}))))
(deftest event->param-map-test
  (is (= {:step 4, :pulse 2}
         (event->param-map {:value/type :pulse :value 2 :step 4
                            :arc/active [0 1/3]}))))

(deftest apply-pat-to-pat-both-test
  (let [op +
        main-events [{:value/type :sound :value "bd"
                      :arc/active [0 1/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [1/3 2/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [2/3 1] :note 1}]
        op-events [{:value/type :note :value 4
                    :arc/active [0 1/2]}
                   {:value/type :note :value 5
                    :arc/active [1/2 1]}]]
    (is (= [{:value/type :sound, :value "bd", :arc/active [0 1/3], :note 5}
            {:value/type :sound, :value "bd", :arc/active [1/3 1/2], :note 5}
            {:value/type :sound, :value "bd", :arc/active [1/2 2/3], :note 6}
            {:value/type :sound, :value "bd", :arc/active [2/3 1], :note 6}]
           (apply-pat-to-pat-both op main-events op-events)))))

(deftest apply-pat-to-pat-left-test
  (let [op +
        main-events [{:value/type :sound :value "bd"
                      :arc/active [0 1/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [1/3 2/3] :note 1}
                     {:value/type :sound :value "bd"
                      :arc/active [2/3 1] :note 1}]
        op-events [{:value/type :note :value 4
                    :arc/active [0 1/6]}
                   {:value/type :note :value 4
                    :arc/active [1/6 1/3]}
                   {:value/type :note :value 5
                    :arc/active [1/3 1]}]]
    (is (= [{:value/type :sound, :value "bd", :arc/active [0 1/3], :note 5}
            {:value/type :sound, :value "bd", :arc/active [1/3 2/3], :note 6}
            {:value/type :sound, :value "bd", :arc/active [2/3 1], :note 6}]
           (apply-pat-to-pat-left op main-events op-events)))))

(deftest merge-pat-to-pat-left-test
  (testing "Applying the patterned param values of euclidean to a sound pattern"
    (let [op +
          main-events [{:value/type :sound :value "bd"
                        :arc/active [0 1/3]}
                       {:value/type :sound :value "cp"
                        :arc/active [1/3 2/3]}
                       {:value/type :sound :value "hh"
                        :arc/active [2/3 1]}]
          step-events [{:value/type :step :value 4
                        :arc/active [0 1/2]}
                       {:value/type :step :value 5
                        :arc/active [1/2 1]}
                       {:value/type :step :value 4
                        :arc/active [1 3/2]}
                       {:value/type :step :value 5
                        :arc/active [3/2 2]}]
          pulse-events [{:value/type :pulse :value 2
                         :arc/active [0 1/3]}
                        {:value/type :pulse :value 3
                         :arc/active [1/3 2/3]}
                        {:value/type :pulse :value 5
                         :arc/active [2/3 1]}
                        {:value/type :pulse :value 2
                         :arc/active [1 4/3]}
                        {:value/type :pulse :value 3
                         :arc/active [4/3 5/3]}
                        {:value/type :pulse :value 5
                         :arc/active [5/3 2]}]
          rotation-events [{:value/type :rotation :value 0
                            :arc/active [0 1]}
                           {:value/type :rotation :value 0
                            :arc/active [1 2]}]
          param-events (reduce (fn [apped-events new-events]
                                 (apply-pat-to-pat-both op apped-events new-events))
                               [step-events pulse-events rotation-events])]
      (is (= [{:value/type :sound, :value "bd", :arc/active [0 1/3], :pulse 2, :rotation 0, :step 4}
              {:value/type :sound, :value "cp", :arc/active [1/3 2/3], :pulse 3, :rotation 0, :step 4}
              {:value/type :sound, :value "hh", :arc/active [2/3 1], :pulse 5, :rotation 0, :step 5}]
             (merge-pat-to-pat-left main-events param-events))))))
