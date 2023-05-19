(ns piratidal.pattern-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.core :as core]
   [piratidal.parser :refer [parse-pattern]]
   [piratidal.pattern
    :refer
    [palindrome-cycles query remove-silences rev-event]]))

(deftest atom-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1], :arc/active [0 1]}
          {:value/type :sound :value "bd", :arc/whole [1 2], :arc/active [1 2]}]
         (query {:pattern/type :atom :value/type :sound :value "bd"}
                [0 2]))))

(deftest fast-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1/6], :arc/active [0 1/6]}
          {:value/type :sound :value "hh", :arc/whole [1/6 1/3], :arc/active [1/6 1/3]}
          {:value/type :sound :value "cp", :arc/whole [1/3 1/2], :arc/active [1/3 1/2]}
          {:value/type :sound :value "bd", :arc/whole [1/2 2/3], :arc/active [1/2 2/3]}
          {:value/type :sound :value "hh", :arc/whole [2/3 5/6], :arc/active [2/3 5/6]}
          {:value/type :sound :value "cp", :arc/whole [5/6 1], :arc/active [5/6 1]}]
         (query {:pattern/type :fast
                 :speed 2
                 :value {:pattern/type :fastcat
                         :len 3
                         :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                 {:pattern/type :atom :value/type :sound :value "hh"}
                                 {:pattern/type :atom :value/type :sound :value "cp"}]}}
                [0 1]))))

(deftest fastgap-test
  (is (= [["bd" [0N 1/6]]
          ["hh" [1/6 1/3]]
          ["cp" [1/3 1/2]]
          ["bd" [1N 7/6]]
          ["hh" [7/6 4/3]]
          ["sd" [4/3 3/2]]]
         (mapv (juxt :value :arc/active)
               (query {:pattern/type :fastgap
                       :speed 2
                       :value {:pattern/type :fastcat
                               :len 3
                               :value [{:pattern/type :atom :value "bd"}
                                       {:pattern/type :atom :value "hh"}
                                       {:pattern/type :slowcat
                                        :len 2
                                        :value [{:pattern/type :atom :value "cp"}
                                                {:pattern/type :atom :value "sd"}]}]}}
                      [0 2])))))

(deftest slow-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0N 2/3], :arc/active [0N 2/3]}
          {:value/type :sound :value "hh", :arc/whole [2/3 4/3], :arc/active [2/3 4/3]}
          {:value/type :sound :value "cp", :arc/whole [4/3 2N], :arc/active [4/3 2N]}]
         (query {:pattern/type :slow
                 :speed 2
                 :value {:pattern/type :fastcat
                         :len 3
                         :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                 {:pattern/type :atom :value/type :sound :value "hh"}
                                 {:pattern/type :atom :value/type :sound :value "cp"}]}}
                [0 2])
         (->> (query (parse-pattern "[bd hh cp]/2" {:value-type :sound}) [0 2])
              (map #(assoc % :value/type :sound))
              remove-silences)))

  (testing "a single cycle"
    (is (= [{:value/type :sound
             :value "hh"
             :arc/whole [2/3 4/3]
             :arc/active [2/3 4/3]
             :has-start? false}
            {:value/type :sound :value "cp", :arc/whole [4/3 2N], :arc/active [4/3 2N]}]
           (query {:pattern/type :slow
                   :speed 2
                   :value {:pattern/type :fastcat
                           :len 3
                           :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                   {:pattern/type :atom :value/type :sound :value "hh"}
                                   {:pattern/type :atom :value/type :sound :value "cp"}]}}
                  [1 2])))))

(deftest slowcat-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1], :arc/active [0 1]}
          {:value/type :sound :value "hh", :arc/whole [1 2], :arc/active [1 2]}
          {:value/type :sound :value "cp", :arc/whole [2 3], :arc/active [2 3]}]
         (query {:pattern/type :slowcat
                 :len 3
                 :value [{:pattern/type :atom :value/type :sound :value "bd"}
                         {:pattern/type :atom :value/type :sound :value "hh"}
                         {:pattern/type :atom :value/type :sound :value "cp"}]}
                [0 3])))
  (testing "slightly shifted `query-arc`"
    (is (= [{:value/type :sound
             :value "hh"
             :arc/whole [1/3 2/3]
             :arc/active [1/3 2/3]
             :has-start? false}
            {:value/type :sound :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}
            {:value/type :sound :value "bd", :arc/whole [1 4/3], :arc/active [1 4/3]}]
           (query {:pattern/type :fastcat
                   :len 3
                   :value [{:pattern/type :atom :value/type :sound :value "bd"}
                           {:pattern/type :atom :value/type :sound :value "hh"}
                           {:pattern/type :atom :value/type :sound :value "cp"}]}
                  [1/2 4/3])))))

(deftest fastcat-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1/3], :arc/active [0 1/3]}
          {:value/type :sound :value "hh", :arc/whole [1/3 2/3], :arc/active [1/3 2/3]}
          {:value/type :sound :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}]
         (query {:pattern/type :fastcat
                 :len 3
                 :value [{:pattern/type :atom :value/type :sound :value "bd"}
                         {:pattern/type :atom :value/type :sound :value "hh"}
                         {:pattern/type :atom :value/type :sound :value "cp"}]}
                [0 1])))
  (testing "slightly shifted `query-arc`"
    (is (= [{:value/type :sound, :value "hh", :arc/whole [1/3 2/3], :arc/active [1/3 2/3], :has-start? false}
            {:value/type :sound, :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}
            {:value/type :sound, :value "bd", :arc/whole [1 4/3], :arc/active [1 4/3]}]
           (query {:pattern/type :fastcat
                   :len 3
                   :value [{:pattern/type :atom :value/type :sound :value "bd"}
                           {:pattern/type :atom :value/type :sound :value "hh"}
                           {:pattern/type :atom :value/type :sound :value "cp"}]}
                  [1/2 4/3]))))
  (testing "nested fast"
    (is (= [{:value/type :sound :value "bd", :arc/whole [0N 1/3], :arc/active [0N 1/3]}
            {:value/type :sound :value "bd", :arc/whole [1/3 2/3], :arc/active [1/3 1/2]}
            {:value/type :sound :value "cp", :arc/whole [1/2 1], :arc/active [1/2 1]}
            {:value/type :sound
             :value "bd"
             :arc/whole [5/6 7/6]
             :arc/active [5/6 7/6]
             :has-start? false}
            {:value/type :sound :value "bd", :arc/whole [7/6 3/2], :arc/active [7/6 3/2]}
            {:value/type :sound :value "cp", :arc/whole [3/2 2], :arc/active [3/2 2]}]
           (query {:pattern/type :fastcat
                   :len 2
                   :value [{:pattern/type :fast
                            :value {:pattern/type :atom :value/type :sound :value "bd"}
                            :speed 3/2}
                           {:pattern/type :atom :value/type :sound :value "cp"}]}
                  [0 2])))))

(deftest rev-event-test
  (let [no-whole #(dissoc % :arc/whole) ;; FIXME update this
        ]
    (is (= {:value/type :sound :value "bd" :arc/active [1/2 1]}
           (no-whole (rev-event 0 {:value/type :sound :value "bd" :arc/whole [0 1/2] :arc/active [0 1/2]}))))
    (is (= {:value/type :sound :value "bd", :arc/active [0 1]}
           (no-whole (rev-event 0 {:value/type :sound :value "bd" :arc/whole [0 1] :arc/active [0 1]}))))
    (is (= {:value/type :sound :value :silence, :arc/active [-1N 1N], :original-value "bd"}
           (no-whole (rev-event 0 {:value/type :sound :value "bd" :arc/whole [0 2] :arc/active [0 2]}))))
    (is (= {:value/type :sound :value "bd", :arc/active [1 3]}
           (no-whole (rev-event 1 {:value/type :sound :value "bd" :arc/whole [0 2] :arc/active [0 2]}))))
    (is (= {:value/type :sound :value :silence, :arc/active [-1/2 1N], :original-value "bd"}
           (no-whole (rev-event 0 {:value/type :sound :value "bd" :arc/whole [0 3/2] :arc/active [0 3/2]}))))
    (is (= {:value/type :sound :value "bd", :arc/active [3/2 3]}
           (no-whole (rev-event 1 {:value/type :sound :value "bd" :arc/whole [0 3/2] :arc/active [0 3/2]}))))
    (is (= {:value/type :sound :value "bd", :arc/active [5/3 7/3]}
           (no-whole (rev-event 1 {:value/type :sound :value "bd" :arc/whole [2/3 4/3] :arc/active [2/3 4/3]}))))))

(deftest rev-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [3/2 3N], :arc/active [3/2 3N]}
          {:value/type :sound :value "bd", :arc/whole [2N 7/2], :arc/active [2N 7/2]}]
         (remove-silences
          (query {:pattern/type :rev
                  :value {:pattern/type :slow
                          :speed 3/2
                          :value {:pattern/type :atom
                                  :value/type :sound :value "bd"}}}
                 [0 3]))))
  (is (= [{:value/type :sound :value :silence :arc/whole [-1/2 1N] :arc/active [0N 1N] :original-value "bd"}
          {:value/type :sound :value :silence :arc/whole [0N 3/2] :arc/active [1N 3/2] :original-value "bd"}
          {:value/type :sound :value "bd", :arc/whole [3/2 3N], :arc/active [3/2 3N]}
          {:value/type :sound :value "bd", :arc/whole [2N 7/2], :arc/active [2N 7/2]}]
         (query {:pattern/type :rev
                 :value {:pattern/type :slow
                         :speed 3/2
                         :value {:pattern/type :atom
                                 :value/type :sound :value "bd"}}}
                [0 3])))
  (is (= [{:value/type :sound :value "bd", :arc/whole [1N 3N], :arc/active [1N 3N]}]
         (query {:pattern/type :rev
                 :value {:pattern/type :slow
                         :speed 2
                         :value {:pattern/type :atom
                                 :value/type :sound :value "bd"}}}
                [1 2]))))

(deftest palindrome-cycles-test
  (is (= [{:cycle 0, :arc [0 1]}
          {:cycle 1, :arc [0 1]}
          {:cycle 2, :arc [1 2]}
          {:cycle 3, :arc [1 2]}]
         (palindrome-cycles [1/2 4]))))

(deftest palindrome-test
  (is (= [["bd" [0N 1N]]
          ["bd" [3N 5N]]]
         (mapv (juxt :value :arc/active)
               (remove-silences
                (query {:pattern/type :palindrome
                        :value {:pattern/type :slow
                                :speed 2
                                :value {:pattern/type :atom :value/type :sound :value "bd"}}}
                       [0 4])))))
  (is (= [["bd" [0 1/3]]
          ["cp" [1/3 2/3]]
          ["hh" [2/3 1N]]
          ["hh" [1N 4/3]]
          ["cp" [4/3 5/3]]
          ["bd" [5/3 2N]]
          ["bd" [2 7/3]]
          ["cp" [7/3 8/3]]
          ["hh" [8/3 3N]]]
         (mapv (juxt :value :arc/active)
               (remove-silences
                (query {:pattern/type :palindrome
                        :value {:pattern/type :fastcat
                                :len 3
                                :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                        {:pattern/type :atom :value/type :sound :value "cp"}
                                        {:pattern/type :atom :value/type :sound :value "hh"}]}}
                       [0 3])))))
  (is (= [["bd" [0 1/6]]
          ["cp" [1/6 2/6]]
          ["hh" [2/6 3/6]]
          ["bd" [3/6 4/6]]
          ["cp" [4/6 5/6]]
          ["hh" [5/6 6/6]]
          ["hh" [6/6 7/6]]
          ["cp" [7/6 8/6]]
          ["bd" [8/6 9/6]]
          ["hh" [9/6 10/6]]
          ["cp" [10/6 11/6]]
          ["bd" [11/6 12/6]]]
         (-> (core/sound "[bd cp hh]*2")
             core/palindrome
             (query [0 2])
             (->> remove-silences
                  (mapv (juxt :value :arc/active))))
         (mapv (juxt :value :arc/active)
               (remove-silences
                (query {:pattern/type :palindrome
                        :value {:pattern/type :with-param-pattern
                                :value {:pattern/type :fast
                                        :value {:pattern/type :fastcat
                                                :len 3
                                                :value [{:pattern/type :atom, :value "bd", :value/type :s}
                                                        {:pattern/type :atom, :value "cp", :value/type :s}
                                                        {:pattern/type :atom, :value "hh", :value/type :s}]}}
                                :pattern/params [{:pattern/type :atom, :value 2, :value/type :speed}]}}
                       [0 2]))))))

(deftest superimpose-test
  (is (= [{:value/type :sound, :value "bd", :arc/whole [0 1], :arc/active [0 1]}
          {:value/type :sound, :value "bd", :arc/whole [0 1], :arc/active [0 1], :gain 1}]
         (query {:pattern/type :superimpose
                 :value {:pattern/type :atom, :value "bd" :value/type :sound}
                 :f (fn [v] {:pattern/type :control-pattern
                             :controls [{:pattern/type :atom, :value 1 :value/type :gain}]
                             :value v})}
                [0 1]))))

(deftest control-pattern-test
  (is (= [{:value/type :sound, :value "bd", :arc/whole [0 1], :arc/active [0 1], :gain 1}]
         (query {:pattern/type :control-pattern
                 :controls [{:pattern/type :fastcat
                             :len 3
                             :value [{:pattern/type :atom, :value 1 :value/type :gain}
                                     {:pattern/type :atom, :value 2 :value/type :gain}
                                     {:pattern/type :atom, :value 3 :value/type :gain}]}]
                 :value {:pattern/type :atom, :value "bd" :value/type :sound}}
                [0 1])))
  (is (= [{:arc/active [0 1/4], :gain 1, :note 10}
          {:arc/active [1/4 1/2], :gain 1, :note 10}
          {:arc/active [1/2 3/4], :gain 1, :note 10}
          {:arc/active [3/4 1], :gain 1, :note 9}
          {:arc/active [1 5/4], :gain 2, :note 9}
          {:arc/active [5/4 3/2], :gain 2, :note 9}
          {:arc/active [3/2 7/4], :gain 2, :note 8}
          {:arc/active [7/4 2], :gain 2, :note 8}]
         (map #(select-keys % [:arc/active :gain :note])
              (query {:pattern/type :control-pattern
                      :controls [{:pattern/type :slowcat
                                  :len 3
                                  :value [{:pattern/type :atom, :value 1 :value/type :gain}
                                          {:pattern/type :atom, :value 2 :value/type :gain}
                                          {:pattern/type :atom, :value 3 :value/type :gain}]}
                                 {:pattern/type :slow
                                  :speed 2
                                  :value {:pattern/type :fastcat
                                          :len 3
                                          :value [{:pattern/type :atom, :value 10 :value/type :note}
                                                  {:pattern/type :atom, :value 9 :value/type :note}
                                                  {:pattern/type :atom, :value 8 :value/type :note}]}}]
                      :value {:pattern/type :fastcat
                              :len 4
                              :value [{:pattern/type :atom, :value "arpy" :value/type :sound}
                                      {:pattern/type :atom, :value "arpy" :value/type :sound}
                                      {:pattern/type :atom, :value "arpy" :value/type :sound}
                                      {:pattern/type :atom, :value "arpy" :value/type :sound}]}}
                     [0 2])))))

(deftest off-test
  (is (= [{:value/type :sound :value "bd" :arc/whole [0 1/2] :arc/active [0 1/2]}
          {:value/type :sound :value "cp" :arc/whole [1/2 1] :arc/active [1/2 1]}
          {:value/type :sound :value "bd" :arc/whole [1/4 3/4] :arc/active [1/4 3/4] :gain 1}
          {:value/type :sound :value "cp" :arc/whole [3/4 5/4] :arc/active [3/4 5/4] :gain 3}]
         (query {:pattern/type :off
                 :amount 1/4
                 :value {:pattern/type :fastcat
                         :len 2
                         :value [{:pattern/type :atom, :value "bd" :value/type :sound}
                                 {:pattern/type :atom, :value "cp" :value/type :sound}]}
                 :f (fn [v] {:pattern/type :control-pattern
                             :controls [{:pattern/type :fastcat
                                         :len 3
                                         :value [{:pattern/type :atom, :value 1 :value/type :gain}
                                                 {:pattern/type :atom, :value 2 :value/type :gain}
                                                 {:pattern/type :atom, :value 3 :value/type :gain}]}]
                             :value v})}
                [0 1]))))

(deftest rotl-test
  (is (= [{:value/type :note, :value 1, :arc/whole [1/4 3/4], :arc/active [1/4 3/4]}
          {:value/type :note, :value 2, :arc/whole [3/4 5/4], :arc/active [3/4 5/4]}]
         (query {:pattern/type :rotl
                 :amount -1/4
                 :value {:pattern/type :fastcat
                         :len 2
                         :value [{:pattern/type :atom :value 1 :value/type :note}
                                 {:pattern/type :atom :value 2 :value/type :note}]}}
                [0 1]))))

(deftest rotr-test
  (is (= [[1 [1/3 5/6]]
          [2 [5/6 4/3]]
          [1 [4/3 11/6]]
          [2 [11/6 7/3]]]
         (map (juxt :value :arc/active)
              (query {:pattern/type :rotr
                      :amount 1/3
                      :value {:pattern/type :fastcat
                              :len 2
                              :value [{:pattern/type :atom :value 1}
                                      {:pattern/type :atom :value 2}
                                      #_{:pattern/type :atom :value 3}
                                      #_{:pattern/type :atom :value 4}]}}
                     [0 2])))))

;; TODO this certainly could be improved
(deftest somecycles-by-test
  (testing "In 10K iterations with probability 50%, then the value lies within 50%+-5%"
    (is (every? (fn [[_ v]] (< (* 5000 0.95) v (* 5000 1.05)))
                (frequencies
                 (map :value
                      (query {:pattern/type :somecycles-by
                              :probability 0.5
                              :f (fn [_] {:pattern/type :silence})
                              :value {:pattern/type :atom :value "bd"}}
                             [0 10000])))))))

(deftest degrade-by-test
  (testing "In 10K iterations with probability 50%, then the value lies within 50%+-5%"
    (is (every? (fn [[_ v]] (< (* 5000 0.95) v (* 5000 1.05)))
                (frequencies
                 (map :value
                      (query {:pattern/type :degrade-by
                              :value {:pattern/type :atom :value "cp"}
                              :probability 0.5}
                             [0 10000])))))))

(deftest layer-test
  (is (= [;; rev
          ["cp" [0N 1/2]]
          ["bd" [1/2 1N]]
          ;; fast
          ["bd" [0 1/6]]
          ["cp" [1/6 1/3]]
          ["bd" [1/3 1/2]]
          ["cp" [1/2 2/3]]
          ["bd" [2/3 5/6]]
          ["cp" [5/6 1]]]
         (map (juxt :value :arc/active)
              (query {:pattern/type :layer
                      :fs [(fn [v] {:pattern/type :rev :value v})
                           (fn [v] {:pattern/type :fast :speed 3 :value v})]
                      :value {:pattern/type :fastcat
                              :len 2
                              :value [{:pattern/type :atom :value "bd"}
                                      {:pattern/type :atom :value "cp"}]}}
                     [0 1])))))

(deftest jux-test
  (is (= [;; fast events
          ["bd" [0 1/6] 0]
          ["cp" [1/6 1/3] 0]
          ["bd" [1/3 1/2] 0]
          ["cp" [1/2 2/3] 0]
          ["bd" [2/3 5/6] 0]
          ["cp" [5/6 1] 0]
          ;; original events
          ["bd" [0 1/2] 1]
          ["cp" [1/2 1] 1]]
         (map (juxt :value :arc/active :pan)
              (query {:pattern/type :jux
                      :f (fn [v] {:pattern/type :fast :speed 3 :value v})
                      :value {:pattern/type :fastcat
                              :len 2
                              :value [{:pattern/type :atom :value "bd"}
                                      {:pattern/type :atom :value "cp"}]}}
                     [0 1])))))

(deftest euclidean-test
  (is (= [["cp" [0 1/8]]
          [:silence [1/8 1/4]]
          [:silence [1/4 3/8]]
          ["cp" [3/8 1/2]]
          [:silence [1/2 5/8]]
          [:silence [5/8 3/4]]
          ["cp" [3/4 7/8]]
          [:silence [7/8 1]]]
         (map (juxt :value :arc/active)
              (query {:pattern/type :euclidean
                      :pulses 3
                      :steps 8
                      :rotation 0
                      :value {:pattern/type :atom, :value "cp" :value/type :sound}}
                     [0 1]))))
  (is (= [[:silence [0 1/8]]
          [:silence [1/8 1/4]]
          ["cp" [1/4 3/8]]
          [:silence [3/8 1/2]]
          [:silence [1/2 5/8]]
          ["cp" [5/8 3/4]]
          [:silence [3/4 7/8]]
          ["cp" [7/8 1]]]
         (map (juxt :value :arc/active)
              (query {:pattern/type :euclidean
                      :pulses 3
                      :steps 8
                      :rotation 1
                      :value {:pattern/type :atom, :value "cp" :value/type :sound}}
                     [0 1])))))

(deftest polymeter-test
  (is (= [["hh" [0 1/2]]
          ["a" [0 1/2]]
          ["cp" [1/2 1]]
          ["b" [1/2 1]]
          ["hh" [1 3/2]]
          ["c" [1 3/2]]
          ["cp" [3/2 2]]
          ["A" [3/2 2]]]
         (mapv (juxt :value :arc/active)
               (sort-by (comp first :arc/active)
                        (query {:pattern/type :polymeter
                                :len 2
                                :value [[{:pattern/type :atom, :value "hh" :value/type :sound}
                                         {:pattern/type :atom, :value "cp" :value/type :sound}]
                                        [{:pattern/type :slowcat
                                          :len 2
                                          :value [{:pattern/type :atom, :value "a" :value/type :sound}
                                                  {:pattern/type :atom, :value "A" :value/type :sound}]}
                                         {:pattern/type :atom, :value "b" :value/type :sound}
                                         {:pattern/type :atom, :value "c" :value/type :sound}]]}
                               [0 2]))))))

(deftest with-param-pattern-test
  (is (= [["cp" [0 1/8] nil]
          [:silence [1/8 1/4] nil]
          [:silence [1/4 3/8] nil]
          ["cp" [3/10 2/5] false]
          [:silence [2/5 1/2] nil]
          [:silence [1/2 3/5] nil]
          ["cp" [3/5 7/10] nil]
          ["cp" [2/3 3/4] nil]
          [:silence [3/4 5/6] nil]
          [:silence [5/6 11/12] nil]
          [:silence [11/12 1] nil]]
         ;; TODO make a generative test that ensures that the events that don't have `:has-start? false` all start one after the other
         (mapv (juxt :value :arc/active :has-start?)
               (query {:pattern/type :with-param-pattern
                       :pattern/params [{:pattern/type :fastcat
                                         :len 3
                                         :value [{:pattern/type :atom :value 8 :value/type :steps}
                                                 {:pattern/type :atom :value 10 :value/type :steps}
                                                 {:pattern/type :atom :value 12 :value/type :steps}]}
                                        {:pattern/type :slowcat
                                         :len 3
                                         :value [{:pattern/type :atom :value 3 :value/type :pulses}
                                                 {:pattern/type :atom :value 4 :value/type :pulses}
                                                 {:pattern/type :atom :value 5 :value/type :pulses}]}
                                        {:pattern/type :atom :value 0 :value/type :rotation}]
                       :value {:pattern/type :euclidean
                               :value {:pattern/type :atom, :value "cp" :value/type :sound}}}
                      [0 1])))))
