(ns tidal-mini.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.control-patterns :refer [gain note]]
   [tidal-mini.core
    :refer [make-schedule polymeter->stack polymeter-step-at-cycle&index]]
   [tidal-mini.parser :refer [parse-pattern]]))

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
  (testing "Will calculate the current `:alt` cycle"
    (is (= {:stack
            [[{:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
              {:word "a"}
              {:word "b"}
              {:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}]]}
           (polymeter->stack
            0
            {:polymeter
             {:stack [[{:alt [{:stack [[{:word "c"}
                                        {:word "d"}]]}]}
                       {:word "a"}
                       {:word "b"}]]}
             :steps 4})))
    (testing "and will not confuse the `:alt` indexes"
      (is (= {:stack
              [[{:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
                {:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 0}
                {:word "b"}
                {:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}]]}
             (polymeter->stack
              0
              {:polymeter
               {:stack [[{:alt [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:alt [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:word "b"}]]}
               :steps 4}))))
    (testing "will work for cycles other than `0`"
      (is (= {:stack
              [[{:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 1}
                {:word "b"}
                {:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 2}
                {:alt [{:stack [[{:word "c"} {:word "d"}]]}], :cycle 2}]]}
             (polymeter->stack
              1
              {:polymeter
               {:stack [[{:alt [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:alt [{:stack [[{:word "c"} {:word "d"}]]}]}
                         {:word "b"}]]}
               :steps 4}))))))

(deftest polymeter-step-at-cycle&index-test
  (testing
   "A polymeter such as {0 1}%3
       0     1     2       ; cycle
       0 1 2 0 1 2 0 1 2   ; current-step-index
       1 2 1 2 1 2 1 2 1   ; current-polymeter-step
       0 1 2 3 4 5 6 7 8   ; event-index
       0 0 1 1 2 2 3 3 4   ; times-seen"
    (is (= [{:event-index 0, :times-seen 0}
            {:event-index 1, :times-seen 0}
            {:event-index 2, :times-seen 1}
            {:event-index 3, :times-seen 1}
            {:event-index 4, :times-seen 2}
            {:event-index 5, :times-seen 2}
            {:event-index 6, :times-seen 3}
            {:event-index 7, :times-seen 3}
            {:event-index 8, :times-seen 4}]
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
       0 1 2 3 4 5 6 7 8   ; event-index
       0 0 0 1 1 1 2 2 2   ; times-seen"
    (is (= [{:event-index 0, :times-seen 0}
            {:event-index 1, :times-seen 0}
            {:event-index 2, :times-seen 0}
            {:event-index 3, :times-seen 1}
            {:event-index 4, :times-seen 1}
            {:event-index 5, :times-seen 1}
            {:event-index 6, :times-seen 2}
            {:event-index 7, :times-seen 2}
            {:event-index 8, :times-seen 2}]
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

(deftest make-schedule-test
  (let [pat->schedule (fn [pattern cycles]
                        (mapcat (fn [cycle] (->> pattern
                                                 parse-pattern
                                                 (make-schedule {:index 0 :elapsed-arc 0 :cycle cycle})))
                                cycles))
        sched->word-str #(->> %
                              (map (comp :word :event))
                              (str/join " "))
        remove-commas #(str/replace % "," "")]
    (testing ""
      (is (= [{:event {:word "bd"}, :arc [0 1/3]}
              {:event :silence, :arc [1/3 2/3]}
              {:event {:word "bd"}, :arc [2/3 1]}]
             (pat->schedule "bd ~ bd" [0])))
      (is (= [{:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "bd"}, :arc [1/2 3/4]}
              {:event {:word "sn"}, :arc [3/4 1N]}
              {:event {:word "hh"}, :arc [3/4 7/8]}
              {:event {:word "bd"}, :arc [7/8 1N]}]
             (pat->schedule "bd [bd [sn , [hh bd]]]" [0])))
      (is (= [{:event {:word "hh"}, :arc [0 1N]}
              {:event {:word "sn"}, :arc [0 1N]}]
             (pat->schedule "<hh sn>" [0 1])))

      (is (= [{:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "hh"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "sn"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "hh"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "sn"}, :arc [1/2 1N]}]
             (pat->schedule "bd <hh sn>" [0 1 2 3])))
      (is (= [{:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "tom"}, :arc [1/2 1N]}]
             (pat->schedule "bd <hh <sn tom>>" [3])))
      (is (= [{:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "hh"}, :arc [1/2 1N]}
              {:event {:word "tom"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "bd"}, :arc [1/2 1N]}
              {:event {:word "sn"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "hh"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [1/2 1N]}]
             (pat->schedule "bd <hh bd, tom sn bd>" (range 3))))
      (is (= [{:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "sn"}, :arc [1/2 1N]}
              {:event {:word "tom"}, :arc [1/2 3/4]}
              {:event {:word "sn"}, :arc [3/4 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "hh"}, :arc [1/2 1N]}
              {:event {:word "sd"}, :arc [1/2 1N]}
              {:event {:word "bd"}, :arc [0 1/2]}
              {:event {:word "sn"}, :arc [1/2 1N]}
              {:event {:word "tom"}, :arc [1/2 3/4]}
              {:event {:word "sn"}, :arc [3/4 1N]}]
             (pat->schedule "bd <sn hh, [tom sn] sd>" (range 3))))
      (testing "`:polymeter`"
        (is (= [{:event {:word "hh"}, :arc [0 1/3]}
                {:event {:word "sn"}, :arc [1/3 2/3]}
                {:event {:word "sd"}, :arc [2/3 1N]}]
               (pat->schedule "{hh sn sd}" (range 1))))
        (is (= [{:event {:word "bd"}, :arc [0 1/2]}
                {:event {:word "hh"}, :arc [1/2 2/3]}
                {:event {:word "sn"}, :arc [2/3 5/6]}
                {:event {:word "sd"}, :arc [5/6 1N]}]
               (pat->schedule "bd {hh sn sd}" (range 1))))
        (is (= [{:event {:word "a"}, :arc [0 1/4]}
                {:event {:word "b"}, :arc [1/4 1/2]}
                {:event {:word "c"}, :arc [1/2 3/4]}
                {:event {:word "a"}, :arc [3/4 1N]}
                {:event {:word "b"}, :arc [0 1/4]}
                {:event {:word "c"}, :arc [1/4 1/2]}
                {:event {:word "a"}, :arc [1/2 3/4]}
                {:event {:word "b"}, :arc [3/4 1N]}]
               (pat->schedule "{a b c}%4" (range 2))))
        (is (= [{:event {:word "a"}, :arc [0 1/4]}
                {:event {:word "b"}, :arc [1/4 1/2]}
                {:event {:word "c"}, :arc [1/2 3/4]}
                {:event {:word "a"}, :arc [3/4 1N]}
                {:event {:word "b"}, :arc [0 1/4]}
                {:event {:word "d"}, :arc [1/4 1/2]}
                {:event {:word "a"}, :arc [1/2 3/4]}
                {:event {:word "b"}, :arc [3/4 1N]}
                {:event {:word "c"}, :arc [0 1/4]}
                {:event {:word "a"}, :arc [1/4 1/2]}
                {:event {:word "b"}, :arc [1/2 3/4]}
                {:event {:word "d"}, :arc [3/4 1N]}]
               (pat->schedule "{a b <c d>}%4" (range 3))))
        (let [pat "{bd <hh cp crash>}%3"]
          (is (= "bd hh bd cp bd crash"
                 (->> (pat->schedule pat (range 2))
                      sched->word-str))))
        (let [pat "[{bd <hh cp crash>}%3]!2"]
          (is (= "bd hh bd bd hh bd cp bd crash cp bd crash"
                 (->> (pat->schedule pat (range 2))
                      sched->word-str))))
        (let [pat "[{bd <hh cp crash>}%3]*2"]
          (is (= "bd hh bd cp bd crash bd hh bd cp bd crash"
                 (->> (pat->schedule pat (range 2))
                      sched->word-str)))))
      (testing "`:euclidean`"
        (is (= [{:event {:word "hh"}, :arc [0 1/8]}
                {:event :silence, :arc [1/8 1/4]}
                {:event :silence, :arc [1/4 3/8]}
                {:event {:word "hh"}, :arc [3/8 1/2]}
                {:event :silence, :arc [1/2 5/8]}
                {:event :silence, :arc [5/8 3/4]}
                {:event {:word "hh"}, :arc [3/4 7/8]}
                {:event :silence, :arc [7/8 1N]}]
               (pat->schedule "hh(3, 8)" [0]))))
      (testing "`:replicate`"
        (is (= [{:event {:word "hh"}, :arc [0 1/3]}
                {:event {:word "hh"}, :arc [1/3 2/3]}
                {:event {:word "bd"}, :arc [2/3 1N]}]
               (pat->schedule "hh!2 bd" [0]))))
      (testing "`:elongate`"
        (is (= [{:event {:word "hh"}, :arc [0 2/3]}
                {:event {:word "bd"}, :arc [2/3 1N]}]
               (pat->schedule "hh@2 bd" [0])))
        (is (= [{:event {:word "hh"}, :arc [0 1/3]}
                {:event {:word "sn"}, :arc [1/3 2/3]}
                {:event {:word "bd"}, :arc [2/3 1N]}]
               (pat->schedule "[hh sn]@2 bd" [0])))
        (is (= [{:event {:word "bd"}, :arc [0 2/9]}
                {:event {:word "hh"}, :arc [2/9 4/9]}
                {:event {:word "sn"}, :arc [4/9 2/3]}
                {:event {:word "bd"}, :arc [2/3 1N]}]
               (pat->schedule "[bd hh sn]@2 bd" [0]))))
      (testing "`:slow`"
        ;; TODO more tests are needed here
        (is (= [{:event {:word "a"}, :arc [0 1]}]
               (pat->schedule "a/2" (range 2))))
        (is (= [{:event {:word "a"}, :arc [0 1N]}
                {:event {:word "b"}, :arc [0 1]}]
               (pat->schedule "[a b]/2" (range 2))))
        (is (= [{:event {:word "a"}, :arc [0 1]}
                {:event {:word "b"}, :arc [0 1]}
                {:event {:word "a"}, :arc [0 1]}]
               (pat->schedule "[a b]/2" (range 3))))
        (is (= [{:event {:word "a"}, :arc [0N 1/2]}
                {:event {:word "c"}, :arc [1/2 1N]}
                {:event {:word "b"}, :arc [0N 1/2]}
                {:event {:word "c"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0N 1/2]}
                {:event {:word "c"}, :arc [1/2 1N]}]
               (pat->schedule "[a b]/2 c" (range 3))))
        (is (= [{:event {:word "a"}, :arc [0N 1/2]}
                {:event {:word "c"}, :arc [1/2 1N]}
                {:event {:word "b"}, :arc [1/4 1/2]}
                {:event {:word "c"}, :arc [1/2 1N]}
                {:event {:word "c"}, :arc [1/2 1N]}]
               (pat->schedule "[a b]/3 c" (range 3))))
        (is (= [{:event {:word "c"}, :arc [0 1/2]}
                {:event {:word "a"}, :arc [1/2 1N]}
                {:event {:word "c"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [3/4 1N]}
                {:event {:word "c"}, :arc [0 1/2]}
                {:event {:word "c"}, :arc [0 1/2]}
                {:event {:word "a"}, :arc [1/2 1N]}
                {:event {:word "c"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [3/4 1N]}
                {:event {:word "c"}, :arc [0 1/2]}]
               (pat->schedule "c [a b]/3" (range 6))))
        (is (= (remove-commas "a b a c a, a b a d a") ; cycles separated by commas
               (sched->word-str (pat->schedule "a [b <c d>]/3" (range 6)))))
        (is (= (remove-commas "a b a c a, a b a d a, a b a e a")
               (sched->word-str (pat->schedule "a [b <c d e>]/3" (range 9)))))
        (is (= [{:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "c"}, :arc [3/4 1N]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "d"}, :arc [3/4 1N]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "e"}, :arc [3/4 1]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "f"}, :arc [5/8 1]}
                {:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}]
               (pat->schedule "a [b <c d [e f]>]/3" (range 10))))
        (is (= "a b a c a a b a d a a b a e a a b a f a"
               (sched->word-str (pat->schedule "a [b <<c f> d e>]/3" (range 12))))))
      (testing "`:fast`"
        (is (= [{:event {:word "a"}, :arc [0 1/2]}
                {:event {:word "a"}, :arc [1/2 1]}]
               (pat->schedule "a*2" [0])))
        (is (= [{:event {:word "a"}, :arc [0 1/4]}
                {:event {:word "a"}, :arc [1/4 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}]
               (pat->schedule "a*2 b" [0])))
        (is (= [{:event {:word "a"}, :arc [0 1/4]}
                {:event {:word "c"}, :arc [1/4 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0 1/4]}
                {:event {:word "c"}, :arc [1/4 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}]
               (pat->schedule "<a c>*2 b" (range 2))))
        (is (= [{:event {:word "a"}, :arc [0N 1/8]}
                {:event {:word "c"}, :arc [1/8 1/4]}
                {:event {:word "a"}, :arc [1/4 3/8]}
                {:event {:word "c"}, :arc [3/8 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}
                {:event {:word "a"}, :arc [0N 1/8]}
                {:event {:word "c"}, :arc [1/8 1/4]}
                {:event {:word "a"}, :arc [1/4 3/8]}
                {:event {:word "c"}, :arc [3/8 1/2]}
                {:event {:word "b"}, :arc [1/2 1N]}]
               (pat->schedule "[a c]*2 b" (range 2))))
        (is (= [{:event {:word "hh"}, :arc [0N 1/6]}
                {:event {:word "cp"}, :arc [1/6 1/3]}
                {:event {:word "hh"}, :arc [1/3 1/2]}
                {:event {:word "bd"}, :arc [1/2 1N]}
                {:event {:word "cp"}, :arc [0N 1/6]}
                {:event {:word "hh"}, :arc [1/6 1/3]}
                {:event {:word "cp"}, :arc [1/3 1/2]}
                {:event {:word "bd"}, :arc [1/2 1N]}]
               (pat->schedule "<hh cp>*3 bd" (range 2)))))
      (testing "`:degrade`"
        ;; TODO
        )
      (testing "`:choose`"
        ;; TODO
        )
      (testing "Numeric patterns"
        (testing "integers"
          (is (= [{:event 1, :arc [0 1/3]}
                  {:event 2, :arc [1/3 2/3]}
                  {:event 3, :arc [2/3 1N]}]
                 (pat->schedule "1 2 3" [0]))))
        (testing "floats"
          (is (= [{:event 1, :arc [0 1/3]}
                  {:event 0.2, :arc [1/3 2/3]}
                  {:event 3, :arc [2/3 1N]}]
                 (pat->schedule "1 0.2 3" [0]))))
        (testing "`:alt`"
          (is (= [{:event 1, :arc [0 1/3]}
                  {:event 0.2, :arc [1/3 2/3]}
                  {:event 3, :arc [2/3 1N]}
                  {:event 1, :arc [0 1/3]}
                  {:event 0.7, :arc [1/3 2/3]}
                  {:event 3, :arc [2/3 1N]}]
                 (pat->schedule "1 <0.2 0.7> 3" (range 2))))))
      (testing "Control patterns"
        (let [pat->schedule2 (fn [pattern cycles]
                               (mapcat (fn [cycle]
                                         (->> pattern
                                              (make-schedule {:index 0 :elapsed-arc 0 :cycle cycle})))
                                       cycles))]
          (testing "gain"
            (is (= [{:event {:word "a"}, :arc [0 1/3], :gain 1}
                    {:event {:word "b"}, :arc [1/3 2/3], :gain 2}
                    {:event {:word "c"}, :arc [2/3 1N], :gain 3}]
                   (-> (parse-pattern "a b c")
                       (gain "1 2 3")
                       (pat->schedule2 (range 1)))))
            (is (= [{:event {:word "bd"}, :arc [0 1/3], :gain 1}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 2}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 3}
                    {:event {:word "bd"}, :arc [0 1/3], :gain 1}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 1.5}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 3}]
                   (-> (parse-pattern "bd hh sn")
                       (gain "1 <2 1.5> 3")
                       (pat->schedule2 (range 2)))))
            (is (= [{:event {:word "bd"}, :arc [0 1/3], :gain 1}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 1}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 2}
                    {:event {:word "bd"}, :arc [0 1/3], :gain 1}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 1}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 2}]
                   (-> (parse-pattern "bd hh sn")
                       (gain "1 2")
                       (pat->schedule2 (range 2)))))
            (is (= [{:event {:word "bd"}, :arc [0 2/3], :gain 1}
                    {:event {:word "hh"}, :arc [2/3 1], :gain 2}
                    {:event {:word "sn"}, :arc [1/3 1N], :gain 1}]
                   (-> (parse-pattern "[bd hh sn]/2")
                       (gain "1 2")
                       (pat->schedule2 (range 2))))))
          (testing "note"
            (is (= [{:event {:word "bd"}, :arc [0 2/3], :note 1}
                    {:event {:word "hh"}, :arc [2/3 1], :note 2}
                    {:event {:word "sn"}, :arc [1/3 1N], :note 1}]
                   (-> (parse-pattern "[bd hh sn]/2")
                       (note "1 2")
                       (pat->schedule2 (range 2))))))
          (testing "gain+note"
            (is (= [{:event {:word "bd"}, :arc [0 1/3], :gain 1, :note 12}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 2, :note 12}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 3, :note 13}
                    {:event {:word "bd"}, :arc [0 1/3], :gain 1, :note 12}
                    {:event {:word "hh"}, :arc [1/3 2/3], :gain 2, :note 12}
                    {:event {:word "sn"}, :arc [2/3 1N], :gain 3, :note 13}]
                   (-> (parse-pattern "bd hh sn")
                       (gain "1 2 3")
                       (note "12 13")
                       (pat->schedule2 (range 2)))))))))))
