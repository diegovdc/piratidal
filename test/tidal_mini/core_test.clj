(ns tidal-mini.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.core :refer [make-schedule parse-tidal transform-tree]]))

(deftest parse-tidal-test
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
             (with-out-str (parse-tidal "bd@ bd?0.5" :check-ambiguous? true)))))))

(deftest transform-tree-test
  (testing "Basic pattern"
    (is (= [{:word "a"} :silence {:word "b"}]
           (transform-tree (parse-tidal "a ~ b" :check-ambiguous? true)))))
  (testing "Basic pattern with `x*2`"
    (is (= [[{:stack [[{:word "a"} {:word "a"}]]}]
            :silence
            {:word "b"}]
           (transform-tree (parse-tidal "a*2 ~ b" :check-ambiguous? true))))
    (testing "Is equal to a `:group`"
      (is (= (transform-tree (parse-tidal "[a a] ~ b" :check-ambiguous? true))
             (transform-tree (parse-tidal "a*2 ~ b" :check-ambiguous? true))))))
  (testing "`:group` [a b]}"
    (is (= [{:word "bd"}
            [{:stack [[{:word "bd"}
                       {:word "sn"}
                       {:word "hh"}]]}]]
           (transform-tree (parse-tidal "bd [bd sn hh]" :check-ambiguous? true)))))
  (testing "`:group` [a b] with `x*2`}"
    (is (= [{:word "bd"}
            [{:stack
              [[[{:stack [[{:word "bd"} {:word "sn"} {:word "hh"}]]}]
                [{:stack [[{:word "bd"} {:word "sn"} {:word "hh"}]]}]]]}]]
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
            [{:stack
              [[{:alt [{:stack [[{:word "sn"} {:word "hh"}]]}]}
                {:alt [{:stack [[{:word "sn"} {:word "hh"}]]}]}]]}]]
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

(deftest make-schedule-test
  (let [pat->schedule (fn [pattern cycles]
                        (mapcat (fn [cycle] (->> pattern
                                                 parse-tidal
                                                 transform-tree
                                                 (make-schedule {:index 0 :elapsed-arc 0 :cycle cycle})))
                                cycles))]
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
        (is (= [{:event {:word "bd"}, :arc [0 1/2]}
                {:event {:word "hh"}, :arc [1/2 1N]}
                {:event {:word "bd"}, :arc [0 1/2]}
                {:event {:word "sn"}, :arc [1/2 1N]}
                {:event {:word "bd"}, :arc [0 1/2]}
                {:event {:word "sd"}, :arc [1/2 1N]}]
               (pat->schedule "bd {hh sn sd}" (range 3))))
        (is (= (->> [{:event {:word "hh"}, :arc [0 1N]}
                     {:event {:word "sn"}, :arc [0 1N]}
                     {:event {:word "sd"}, :arc [0 1N]}
                     {:event {:word "hh"}, :arc [0 1N]}
                     {:event {:word "sn"}, :arc [0 1N]}
                     {:event {:word "sd"}, :arc [0 1N]}
                     {:event {:word "hh"}, :arc [0 1N]}
                     {:event {:word "sn"}, :arc [0 1N]}]
                    (repeat 2)
                    flatten)
               (pat->schedule "{hh sn sd}%8" (range 16))))
        (testing "with `:alt`"
          (is (= (->> [{:event {:word "hh"}, :arc [0 1N]}
                       {:event {:word "sn"}, :arc [0 1N]}
                       {:event {:word "sd"}, :arc [0 1N]}
                       {:event {:word "hh"}, :arc [0 1N]}
                       {:event {:word "sn"}, :arc [0 1N]}
                       {:event {:word "bd"}, :arc [0 1N]}
                       {:event {:word "hh"}, :arc [0 1N]}
                       {:event {:word "sn"}, :arc [0 1N]}]
                      (repeat 2)
                      flatten)
                 (pat->schedule "{hh sn <sd bd>}%8" (range 16))))))
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
        ;; TODO
        )
      (testing "`:fast`"
        ;; TODO
        )
      (testing "`:degrade`"
        ;; TODO
        )
      (testing "`:choose`"
        ;; TODO
        ))))
