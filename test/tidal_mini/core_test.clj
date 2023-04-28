(ns tidal-mini.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.core
    :refer
    [make-schedule
     parse-tidal
     polymeter->stack
     polymeter-step-at-cycle&index
     transform-tree]]))

(deftest parse-tidal-test
  (testing
   (testing "Basic pattern"
     (is (= [:pattern [:cat [:word "a"] [:silence "~"] [:word "b"]]]
            (parse-tidal "a ~ b" :check-ambiguous? true))))
    (testing "Basic pattern with `x!2`"
      (is (= [:pattern
              [:cat [:replicate [:word "a"] [:op-replicate [:int "2"]]] [:silence "~"] [:word "b"]]]
             (parse-tidal "a!2 ~ b" :check-ambiguous? true))))
    (testing "`:group` [a b]}"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:group
                [:stack
                 [:pattern
                  [:cat [:word "bd"] [:word "sn"] [:word "hh"]]]]]]]
             (parse-tidal "bd [bd sn hh]" :check-ambiguous? true))))
    (testing "`:group` [a b] with `x!2`}"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:replicate
                [:group
                 [:stack [:pattern [:cat [:word "bd"] [:word "sn"] [:word "hh"]]]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd [bd sn hh]!2" :check-ambiguous? true))))
    (testing "`:group` with `:op-choose` [a | b]}"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:choose
                [:stack [:pattern [:cat [:word "bd"]]]]
                [:stack [:pattern [:cat [:word "sn"]]]]
                [:stack [:pattern [:cat [:word "hh"]]]]]]]
             (parse-tidal "bd [bd | sn | hh]" :check-ambiguous? true))))
    (testing "Nested `:group` [a [a b]]"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:group
                [:stack
                 [:pattern
                  [:cat
                   [:word "bd"]
                   [:group [:stack
                            [:pattern
                             [:cat [:word "sn"] [:word "hh"]]]]]]]]]]]
             (parse-tidal "bd [bd [sn hh]]" :check-ambiguous? true))))
    (testing "`:alt` <a b>"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:alt [:stack
                      [:pattern
                       [:cat
                        [:word "sn"] [:word "hh"]]]]]]]
             (parse-tidal "bd <sn hh>" :check-ambiguous? true))))
    (testing "`:alt` <a b> with `x!2`"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:replicate
                [:alt [:stack [:pattern [:cat [:word "sn"] [:word "hh"]]]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd <sn hh>!2" :check-ambiguous? true))))
    (testing "`:alt` <a b> with `x/2`"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:slow
                [:alt [:stack [:pattern [:cat [:word "sn"] [:word "hh"]]]]]
                [:op-slow [:int "2"]]]]]
             (parse-tidal "bd <sn hh>/2" :check-ambiguous? true))))
    (testing "Nested `:alt` <a <a b>"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:alt
                [:stack
                 [:pattern
                  [:cat
                   [:word "sn"]
                   [:alt
                    [:stack
                     [:pattern
                      [:cat [:word "hh"] [:word "bd"]]]]]]]]]]]
             (parse-tidal "bd <sn <hh bd>>" :check-ambiguous? true))))
    (testing "`:polymeter` {a b c}"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:polymeter
                [:stack
                 [:pattern
                  [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]]]]
             (parse-tidal "bd {bd hh sn}" :check-ambiguous? true))))
    (testing "`:polymeter` {a b c}!2"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:replicate
                [:polymeter
                 [:stack [:pattern [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd {bd hh sn}!2" :check-ambiguous? true))))
    (testing "`:polymeter` with steps {a b c}%8"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:polymeter
                [:stack
                 [:pattern
                  [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]
                [:polymeter-steps [:int "8"]]]]]
             (parse-tidal "bd {bd hh sn}%8" :check-ambiguous? true))))
    (testing "`:polymeter` with steps {a b c}%8!2"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:replicate
                [:polymeter
                 [:stack [:pattern [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]
                 [:polymeter-steps [:int "8"]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd {bd hh sn}%8!2" :check-ambiguous? true))))
    (testing "`:degrade` a?"
      (is (= [:pattern
              [:cat [:word "bd"] [:degrade [:word "bd"] [:op-degrade]]]]
             (parse-tidal "bd bd?" :check-ambiguous? true))))
    (testing "`:degrade` with amount a?0.1"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:degrade [:word "bd"] [:op-degrade [:float "0.1"]]]]]
             (parse-tidal "bd bd?0.1" :check-ambiguous? true))))
    (testing "`:op-elongate`"
      (is (= [:pattern
              [:cat [:elongate [:word "bd"] [:op-elongate [:int "2"]]] [:word "bd"]]]
             (parse-tidal "bd@2 bd" :check-ambiguous? true))))
    (testing "`:op-sample`"
      (is (= [:pattern
              [:cat [:sample [:word "bd"] [:op-sample [:int "2"]]]]]
             (parse-tidal "bd:2" :check-ambiguous? true))))
    (testing "`:op-euclidean`"
      (is (= [:pattern
              [:cat
               [:euclidean
                [:word "bd"]
                [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
             (parse-tidal "bd(3, 8, 1)" :check-ambiguous? true))))
    (testing "`:op-euclidean` with sample"
      (is (= [:pattern
              [:cat
               [:euclidean
                [:sample [:word "bd"] [:op-sample [:int "2"]]]
                [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
             (parse-tidal "bd:2(3, 8, 1)" :check-ambiguous? true))))
    (testing "`:op-euclidean` + `:op-replicate`"
      (is (= [:pattern
              [:cat
               [:replicate
                [:euclidean
                 [:sample [:word "bd"] [:op-sample [:int "2"]]]
                 [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd:2(3, 8, 1)!2" :check-ambiguous? true))))
    (testing "`:op-euclidean` + `:op-replicate` + `:op-elongate`"
      (is (= [:pattern
              [:cat
               [:elongate
                [:replicate
                 [:euclidean
                  [:sample [:word "bd"] [:op-sample [:int "2"]]]
                  [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]
                 [:op-replicate [:int "2"]]]
                [:op-elongate [:int "2"]]]]]
             (parse-tidal "bd:2(3, 8, 1)!2@2" :check-ambiguous? true))))
    (testing "Complex pattern"
      (is (= [:pattern
              [:cat
               [:word "a"]
               [:silence "~"]
               [:fast [:word "b"] [:op-fast [:int "2"]]]
               [:group
                [:stack
                 [:pattern
                  [:cat [:fast [:word "c"] [:op-fast [:int "2"]]] [:word "c"]]]]]
               [:alt [:stack [:pattern [:cat [:word "d"] [:word "e"]]]]]]
              [:cat
               [:replicate
                [:alt [:stack [:pattern [:cat [:word "a"] [:word "b"]]]]]
                [:op-replicate [:int "2"]]]
               [:slow [:word "a"] [:op-slow [:int "2"]]]
               [:group
                [:stack
                 [:pattern [:cat [:word "a"]]]
                 [:pattern
                  [:cat
                   [:alt
                    [:stack
                     [:pattern [:cat [:word "b"]]]
                     [:pattern [:cat [:word "c"]]]]]]]]]]]
             (parse-tidal
              "a ~  b*2 [c*2 c] <d e> . <a b>!2 a/2 [a , <b , c>]" :check-ambiguous? true))))
    (testing "Error handling"
      (testing "Prints somewhat nice errors to the repl"
        (is (= "Parse error at line 1, column 4:
bd@ bd?0.5
   ^
Expected:
#\"[0-9]+\"\n\n"
               (with-out-str (parse-tidal "bd@ bd?0.5" :check-ambiguous? true))))))))

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

(deftest transform-tree-test
  (testing "Basic pattern"
    (is (= [{:word "a"} :silence {:word "b"}]
           (transform-tree (parse-tidal "a ~ b" :check-ambiguous? true)))))
  (testing "Basic pattern with `x*2`"
    (is (= [{:fast {:word "a"}, :speed 2} :silence {:word "b"}]
           (transform-tree (parse-tidal "a*2 ~ b" :check-ambiguous? true)))))
  (testing "`:group` [a b]}"
    (is (= [{:word "bd"}
            [{:stack [[{:word "bd"}
                       {:word "sn"}
                       {:word "hh"}]]}]]
           (transform-tree (parse-tidal "bd [bd sn hh]" :check-ambiguous? true)))))
  (testing "`:group` [a b] with `x*2`}"
    (is (= [{:word "bd"}
            {:fast [{:stack [[{:word "bd"} {:word "sn"} {:word "hh"}]]}]
             :speed 2}]
           (transform-tree (parse-tidal "bd [bd sn hh]*2" :check-ambiguous? true)))))
  ;; TODO
  #_(testing "`:group` with `:op-choose` [a | b]}"
      (is (= [:pattern
              [:cat
               [:word "bd"]
               [:choose
                [:stack [:pattern [:cat [:word "bd"]]]]
                [:stack [:pattern [:cat [:word "sn"]]]]
                [:stack [:pattern [:cat [:word "hh"]]]]]]]
             (parse-tidal "bd [bd | sn | hh]" :check-ambiguous? true))))
  (testing "Nested `:group` [a [a b]]"
    (is (= [{:word "bd"}
            [{:stack [[{:word "bd"}
                       [{:stack [[{:word "sn"} {:word "hh"}]]}]]]}]]
           (transform-tree (parse-tidal "bd [bd [sn hh]]" :check-ambiguous? true)))))
  (testing "Nested `:group` [a [a , b]] with more than one stack"
    (is (= [{:word "bd"}
            [{:stack [[{:word "bd"}
                       [{:stack [[{:word "sn"}]
                                 [{:word "hh"}]]}]]]}]]
           (transform-tree (parse-tidal "bd [bd [sn  , hh]]" :check-ambiguous? true)))))
  (testing "`:alt` <a b>"
    (is (= [{:word "bd"}
            {:alt [{:stack [[{:word "sn"}
                             {:word "hh"}]]}]}]
           (transform-tree (parse-tidal "bd <sn hh>" :check-ambiguous? true)))))
  (testing "`:alt` <a b> with `x*2`"
    (is (= [{:word "bd"}
            {:fast {:alt [{:stack [[{:word "sn"} {:word "hh"}]]}]}, :speed 2}]
           (transform-tree (parse-tidal "bd <sn hh>*2" :check-ambiguous? true)))))
  (testing "Nested `:alt` <a <a b>"
    (is (= [{:word "bd"}
            {:alt
             [{:stack
               [[{:word "sn"}
                 {:alt [{:stack [[{:word "hh"} {:word "bd"}]]}]}]]}]}]
           (transform-tree (parse-tidal "bd <sn <hh bd>>" :check-ambiguous? true)))))
  (testing "`:polymeter` {a b c}"
    (is (= [{:word "bd"}
            {:polymeter {:stack [[{:word "bd"} {:word "hh"} {:word "sn"}]]}
             :steps 3}]
           (transform-tree (parse-tidal "bd {bd hh sn}" :check-ambiguous? true)))))
  (testing "stacked `:polymeter`s {a b c, d e}"
    (is (= [{:word "bd"}
            {:polymeter
             {:stack
              [[{:word "bd"} {:word "hh"} {:word "sn"}]
               [{:word "hh"} {:word "sn"}]]}
             :steps 3}]
           (transform-tree (parse-tidal "bd {bd hh sn, hh sn}" :check-ambiguous? true)))))
  (testing "`:polymeter` with steps {a b c}%8"
    (is (= [{:word "bd"}
            {:polymeter {:stack [[{:word "bd"} {:word "hh"} {:word "sn"}]]}
             :steps 8}]
           (transform-tree (parse-tidal "bd {bd hh sn}%8" :check-ambiguous? true)))))
  (testing "`:polymeter` with steps {a b c}%8"
    (is (= [{:word "bd"}
            {:polymeter
             {:stack
              [[{:word "bd"} {:word "hh"} {:word "sn"}]
               [{:word "hh"}]]}
             :steps 8}]
           (transform-tree (parse-tidal "bd {bd hh sn, hh}%8" :check-ambiguous? true)))))
  (testing "`:degrade` a?"
    (is (= [{:word "bd"} {:degrade {:word "bd"}, :amount 0.5}]
           (transform-tree (parse-tidal "bd bd?" :check-ambiguous? true)))))
  (testing "`:degrade` with amount a?0.1"
    (is (= [{:word "bd"} {:degrade {:word "bd"}, :amount 0.1}]
           (transform-tree (parse-tidal "bd bd?0.1" :check-ambiguous? true))))
    (testing "Supports float notation such as .1")
    (is (= [{:word "bd"} {:degrade {:word "bd"}, :amount 0.1}]
           (transform-tree (parse-tidal "bd bd?.1" :check-ambiguous? true)))))
  (testing "`:op-elongate`"
    (is (= [{:elongated {:word "bd"}, :size 2} {:word "bd"}]
           (transform-tree (parse-tidal "bd@2 bd" :check-ambiguous? true))))
    (is (= [{:word "sn"} {:elongated {:word "bd"}, :size 2} {:word "hh"}]
           (transform-tree (parse-tidal "sn bd@2 hh" :check-ambiguous? true)))))
  (testing "`:op-sample`"
    (is (= [{:sample "bd", :n 2}]
           (transform-tree (parse-tidal "bd:2" :check-ambiguous? true)))))
  (testing "`:op-euclidean`"
    (is (= [{:word "bd"} :silence :silence
            {:word "bd"} :silence :silence
            {:word "bd"} :silence]
           (transform-tree (parse-tidal "bd(3, 8)" :check-ambiguous? true))))
    (is (= [:silence :silence {:word "bd"}
            :silence :silence {:word "bd"}
            :silence {:word "bd"}]
           (transform-tree (parse-tidal "bd(3, 8, 1)" :check-ambiguous? true))))))

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
                                                 parse-tidal
                                                 transform-tree
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
        ))))
