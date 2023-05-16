(ns piratidal.polymeter-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.polymeter
    :refer
    [polymeter->stack polymeter-step-at-cycle&index]]))

(deftest polymeter->stack-test
  (testing "Will return the adecuate stack according to the cycle"
    (let [poly {:polymeter {:stack [[{:word "a"} {:word "b"} {:word "c"}]]}
                :steps 2}]
      (is (= {:stack [[{:word "a"} {:word "b"}]]}
             (polymeter->stack 0 poly)))
      (is (= {:stack [[{:word "c"} {:word "a"}]]}
             (polymeter->stack 1 poly)))
      (is (= {:stack [[{:word "b"} {:word "c"}]]}
             (polymeter->stack 2 poly)))
      (is (= {:stack [[{:word "a"} {:word "b"}]]}
             (polymeter->stack 3 poly)))))
  (testing "Will calculate the current `:slowcat` cycle"
    (is (= {:stack
            [[{:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
              {:word "a"}
              {:word "b"}
              {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}]]}
           (polymeter->stack
            0
            {:polymeter
             {:stack [[{:slowcat [{:stack [[{:word "c"}
                                        {:word "d"}]]}]}
                       {:word "a"}
                       {:word "b"}]]}
             :steps 4})))
    (testing "and will not confuse the `:slowcat` indexes"
      (is (= {:stack
              [[{:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
                {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
                {:word "b"}
                {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}]]}
             (polymeter->stack
              0
              {:polymeter
               {:stack [[{:slowcat [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:word "b"}]]}
               :steps 4}))))
    (testing "will work for cycles other than `0`"
      (is (= {:stack
              [[{:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}
                {:word "b"}
                {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 2}
                {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 2}]]}
             (polymeter->stack
              1
              {:polymeter
               {:stack [[{:slowcat [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:slowcat [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:word "b"}]]}
               :steps 4}))))))

(deftest polymeter-step-at-cycle&index-test
  (testing
   "A polymeter such as {0 1}%3
       0     1     2       ; cycle
       0 1 2 0 1 2 0 1 2   ; current-step-index
       1 2 1 2 1 2 1 2 1   ; current-polymeter-step
       0 1 2 3 4 5 6 7 8   ; value-index
       0 0 1 1 2 2 3 3 4   ; times-seen"
    (is (= [{:value-index 0, :times-seen 0}
            {:value-index 1, :times-seen 0}
            {:value-index 2, :times-seen 1}
            {:value-index 3, :times-seen 1}
            {:value-index 4, :times-seen 2}
            {:value-index 5, :times-seen 2}
            {:value-index 6, :times-seen 3}
            {:value-index 7, :times-seen 3}
            {:value-index 8, :times-seen 4}]
           (map #(apply polymeter-step-at-cycle&index 2 3 %)
                [[0 0]
                 [0 1]
                 [0 2]
                 [1 0]
                 [1 1]
                 [1 2]
                 [2 0]
                 [2 1]
                 [2 2]]))))
  (testing
   "A polymeter such as {0 1 2}%2
       0   1   2   3   4   ; cycle
       0 1 0 1 0 1 0 1 0   ; current-step-index
       1 2 3 1 2 3 1 2 3   ; current-polymeter-step
       0 1 2 3 4 5 6 7 8   ; value-index
       0 0 0 1 1 1 2 2 2   ; times-seen"
    (is (= [{:value-index 0, :times-seen 0}
            {:value-index 1, :times-seen 0}
            {:value-index 2, :times-seen 0}
            {:value-index 3, :times-seen 1}
            {:value-index 4, :times-seen 1}
            {:value-index 5, :times-seen 1}
            {:value-index 6, :times-seen 2}
            {:value-index 7, :times-seen 2}
            {:value-index 8, :times-seen 2}]
           (map #(apply polymeter-step-at-cycle&index 3 2 %)
                [[0 0]
                 [0 1]
                 [1 0]
                 [1 1]
                 [2 0]
                 [2 1]
                 [3 0]
                 [3 1]
                 [4 0]])))))
