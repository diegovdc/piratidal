(ns piratidal.parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.parser :refer [parse-tidal transform-tree]]))

(deftest parse-tidal-test
  (testing
   (testing "Numbers"
     (is (= [:pattern
             [:fastcat [:int "1"] [:float "1.2"] [:int "-1"] [:float "-0.8"]]]
            (parse-tidal "1 1.2 -1 -0.8" :check-ambiguous? true))))
    (testing "Basic pattern"
      (is (= [:pattern [:fastcat [:word "a"] [:silence "~"] [:word "b"]]]
             (parse-tidal "a ~ b" :check-ambiguous? true))))
    (testing "Basic pattern with `x!2`"
      (is (= [:pattern
              [:fastcat [:replicate [:word "a"] [:op-replicate [:int "2"]]] [:silence "~"] [:word "b"]]]
             (parse-tidal "a!2 ~ b" :check-ambiguous? true))))
    (testing "`:group` [a b]}"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:group
                [:stack
                 [:pattern
                  [:fastcat [:word "bd"] [:word "sn"] [:word "hh"]]]]]]]
             (parse-tidal "bd [bd sn hh]" :check-ambiguous? true))))
    (testing "`:group` [a b] with `x!2`}"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:replicate
                [:group
                 [:stack [:pattern [:fastcat [:word "bd"] [:word "sn"] [:word "hh"]]]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd [bd sn hh]!2" :check-ambiguous? true))))
    (testing "`:group` with `:op-choose` [a | b]}"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:choose
                [:stack [:pattern [:fastcat [:word "bd"]]]]
                [:stack [:pattern [:fastcat [:word "sn"]]]]
                [:stack [:pattern [:fastcat [:word "hh"]]]]]]]
             (parse-tidal "bd [bd | sn | hh]" :check-ambiguous? true))))
    (testing "Nested `:group` [a [a b]]"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:group
                [:stack
                 [:pattern
                  [:fastcat
                   [:word "bd"]
                   [:group [:stack
                            [:pattern
                             [:fastcat [:word "sn"] [:word "hh"]]]]]]]]]]]
             (parse-tidal "bd [bd [sn hh]]" :check-ambiguous? true))))
    (testing "`:slowcat` <a b>"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:slowcat [:slowcat-token [:word "sn"] [:word "hh"]]]]]
             (parse-tidal "bd <sn hh>" :check-ambiguous? true))))
    (testing "`:slowcat` <a b> with `x!2`"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:replicate
                [:slowcat [:slowcat-token [:word "sn"] [:word "hh"]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd <sn hh>!2" :check-ambiguous? true))))
    (testing "`:slowcat` <a b> with `x/2`"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:slow
                [:slowcat [:slowcat-token [:word "sn"] [:word "hh"]]]
                [:op-slow [:int "2"]]]]]
             (parse-tidal "bd <sn hh>/2" :check-ambiguous? true))))
    (testing "Nested `:slowcat` <a <a b>"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:slowcat
                [:slowcat-token
                 [:word "sn"]
                 [:slowcat [:slowcat-token [:word "hh"] [:word "bd"]]]]]]]
             (parse-tidal "bd <sn <hh bd>>" :check-ambiguous? true))))
    (testing "`:polymeter` {a b c}"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:polymeter
                [:stack
                 [:pattern
                  [:fastcat [:word "bd"] [:word "hh"] [:word "sn"]]]]]]]
             (parse-tidal "bd {bd hh sn}" :check-ambiguous? true))))
    (testing "`:polymeter` {a b c}!2"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:replicate
                [:polymeter
                 [:stack [:pattern [:fastcat [:word "bd"] [:word "hh"] [:word "sn"]]]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd {bd hh sn}!2" :check-ambiguous? true))))
    (testing "`:polymeter` with steps {a b c}%8"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:polymeter
                [:stack
                 [:pattern
                  [:fastcat [:word "bd"] [:word "hh"] [:word "sn"]]]]
                [:polymeter-steps [:int "8"]]]]]
             (parse-tidal "bd {bd hh sn}%8" :check-ambiguous? true))))
    (testing "`:polymeter` with steps {a b c}%8!2"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:replicate
                [:polymeter
                 [:stack [:pattern [:fastcat [:word "bd"] [:word "hh"] [:word "sn"]]]]
                 [:polymeter-steps [:int "8"]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd {bd hh sn}%8!2" :check-ambiguous? true))))
    (testing "`:degrade` a?"
      (is (= [:pattern
              [:fastcat [:word "bd"] [:degrade [:word "bd"] [:op-degrade]]]]
             (parse-tidal "bd bd?" :check-ambiguous? true))))
    (testing "`:degrade` with amount a?0.1"
      (is (= [:pattern
              [:fastcat
               [:word "bd"]
               [:degrade [:word "bd"] [:op-degrade [:degrade-amount "0.1"]]]]]
             (parse-tidal "bd bd?0.1" :check-ambiguous? true))))
    (testing "`:op-elongate`"
      (is (= [:pattern
              [:fastcat [:elongate [:word "bd"] [:op-elongate [:int "2"]]] [:word "bd"]]]
             (parse-tidal "bd@2 bd" :check-ambiguous? true))))
    (testing "`:op-sample`"
      (is (= [:pattern
              [:fastcat [:sample [:word "bd"] [:op-sample [:int "2"]]]]]
             (parse-tidal "bd:2" :check-ambiguous? true))))
    (testing "`:op-euclidean`"
      (is (= [:pattern
              [:fastcat
               [:euclidean
                [:word "bd"]
                [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
             (parse-tidal "bd(3, 8, 1)" :check-ambiguous? true))))
    (testing "`:op-euclidean` with sample"
      (is (= [:pattern
              [:fastcat
               [:euclidean
                [:sample [:word "bd"] [:op-sample [:int "2"]]]
                [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
             (parse-tidal "bd:2(3, 8, 1)" :check-ambiguous? true))))
    (testing "`:op-euclidean` + `:op-replicate`"
      (is (= [:pattern
              [:fastcat
               [:replicate
                [:euclidean
                 [:sample [:word "bd"] [:op-sample [:int "2"]]]
                 [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]
                [:op-replicate [:int "2"]]]]]
             (parse-tidal "bd:2(3, 8, 1)!2" :check-ambiguous? true))))
    (testing "`:op-euclidean` + `:op-replicate` + `:op-elongate`"
      (is (= [:pattern
              [:fastcat
               [:elongate
                [:replicate
                 [:euclidean
                  [:sample [:word "bd"] [:op-sample [:int "2"]]]
                  [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]
                 [:op-replicate [:int "2"]]]
                [:op-elongate [:int "2"]]]]]
             (parse-tidal "bd:2(3, 8, 1)!2@2" :check-ambiguous? true))))
    (testing "Error handling"
      (testing "Prints somewhat nice errors to the repl"
        (is (= "Parse error at line 1, column 4:
bd@ bd?0.5
   ^
Expected:
#\"-?[0-9]+\"\n\n"
               (with-out-str (parse-tidal "bd@ bd?0.5" :check-ambiguous? true))))))))

(deftest transform-tree-test
  (testing "Basic pattern"
    (is (= {:pattern/type :fastcat
            :len 3
            :value [{:pattern/type :atom, :value "a"}
                    {:pattern/type :atom, :value :silence}
                    {:pattern/type :atom, :value "b"}]}
           (transform-tree (parse-tidal "a ~ b" :check-ambiguous? true))))
    ;; FIXME these numbers should be :pattern/type :atom
    (is (= {:pattern/type :fastcat
            :len 3
            :value
            [{:pattern/type :atom, :value 1}
             {:pattern/type :atom, :value :silence}
             {:pattern/type :atom, :value 2}]}
           (transform-tree (parse-tidal "1 ~ 2" :check-ambiguous? true)))))

  (testing "Basic pattern with `x*2`"
    (is (= {:pattern/type :fastcat
            :len 3
            :value
            [{:pattern/type :with-param-pattern
              :value
              {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
              :pattern/params [{:pattern/type :atom, :value 2, :value/type :speed}]}
             {:pattern/type :atom, :value :silence}
             {:pattern/type :atom, :value "b"}]}
           (transform-tree (parse-tidal "a*2 ~ b" :check-ambiguous? true))))
    (testing "patterned fast"
      (is (= {:pattern/type :fastcat
              :len 3
              :value
              [{:pattern/type :with-param-pattern
                :value
                {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
                :pattern/params [{:pattern/type :slowcat
                                  :len 2
                                  :value [{:pattern/type :atom, :value 2, :value/type :speed}
                                          {:pattern/type :atom, :value 3, :value/type :speed}]}]}
               {:pattern/type :atom, :value :silence}
               {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a*<2 3> ~ b" :check-ambiguous? true))))
      (is (= {:pattern/type :fastcat
              :len 3
              :value
              [{:pattern/type :with-param-pattern
                :value
                {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
                :pattern/params [{:pattern/type :stack
                                  :value [{:pattern/type :slowcat
                                           :len 1
                                           :value [{:pattern/type :atom, :value 2, :value/type :speed}]}
                                          {:pattern/type :slowcat
                                           :len 1
                                           :value
                                           [{:pattern/type :atom, :value 3, :value/type :speed}]}]}]}
               {:pattern/type :atom, :value :silence}
               {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a*<2, 3> ~ b" :check-ambiguous? true))))
      (is (= {:pattern/type :fastcat
              :len 4
              :value [{:pattern/type :with-param-pattern
                       :value
                       {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
                       :pattern/params [{:pattern/type :slowcat
                                         :len 2
                                         :value [{:pattern/type :atom, :value 2, :value/type :speed}
                                                 {:pattern/type :atom, :value 3, :value/type :speed}]}]}
                      {:pattern/type :with-param-pattern
                       :value {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
                       :pattern/params [{:pattern/type :slowcat
                                         :len 2
                                         :value
                                         [{:pattern/type :atom, :value 2, :value/type :speed}
                                          {:pattern/type :atom, :value 3, :value/type :speed}]}]}
                      {:pattern/type :atom, :value :silence}
                      {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a*<2 3>!2 ~ b" :check-ambiguous? true))))
      (is (= {:pattern/type :fastcat
              :len 3
              :value
              [{:pattern/type :with-param-pattern
                :value {:pattern/type :fast, :value {:pattern/type :atom, :value "a"}}
                :pattern/params [{:pattern/type :stack
                                  :value
                                  [{:pattern/type :fastcat
                                    :len 2
                                    :value
                                    [{:pattern/type :atom, :value 2, :value/type :speed}
                                     {:pattern/type :atom, :value 3, :value/type :speed}]}]}]}
               {:pattern/type :atom, :value :silence}
               {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a*[2 3] ~ b" :check-ambiguous? true))))
      (is (= {:pattern/type :fastcat
              :len 3
              :value [{:pattern/type :with-param-pattern
                       :value {:pattern/type :fast
                               :value {:pattern/type :stack
                                       :value [{:pattern/type :fastcat
                                                :len 2
                                                :value [{:pattern/type :atom, :value "a"}
                                                        {:pattern/type :atom, :value "b"}]}]}}
                       :pattern/params [{:pattern/type :stack
                                         :value [{:pattern/type :fastcat
                                                  :len 2
                                                  :value [{:pattern/type :atom, :value 2, :value/type :speed}
                                                          {:pattern/type :atom, :value 3, :value/type :speed}]}]}]}
                      {:pattern/type :atom, :value :silence}
                      {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "[a b]*[2 3] ~ b" :check-ambiguous? true)))))
    (testing "patterned slow"
      (is (= {:pattern/type :fastcat
              :len 3
              :value [{:pattern/type :with-param-pattern
                       :value {:pattern/type :slow, :value {:pattern/type :atom, :value "a"}}
                       :pattern/params [{:pattern/type :slowcat
                                         :len 2
                                         :value [{:pattern/type :atom, :value 2, :value/type :speed}
                                                 {:pattern/type :atom, :value 3, :value/type :speed}]}]}
                      {:pattern/type :atom, :value :silence}
                      {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a/<2 3> ~ b" :check-ambiguous? true))))
      (is (= {:pattern/type :fastcat
              :len 3
              :value [{:pattern/type :with-param-pattern
                       :value {:pattern/type :slow, :value {:pattern/type :atom, :value "a"}}
                       :pattern/params [{:pattern/type :stack
                                         :value [{:pattern/type :slowcat
                                                  :len 1
                                                  :value [{:pattern/type :atom, :value 2, :value/type :speed}]}
                                                 {:pattern/type :slowcat
                                                  :len 1
                                                  :value
                                                  [{:pattern/type :atom, :value 3, :value/type :speed}]}]}]}
                      {:pattern/type :atom, :value :silence}
                      {:pattern/type :atom, :value "b"}]}
             (transform-tree (parse-tidal "a/<2, 3> ~ b" :check-ambiguous? true))))))
  (testing "`:group` [a b]}"
    (is (= {:pattern/type :fastcat
            :len 2
            :value
            [{:pattern/type :atom, :value "bd"}
             {:pattern/type :stack
              :value
              [{:pattern/type :fastcat
                :len 3
                :value
                [{:pattern/type :atom, :value "bd"}
                 {:pattern/type :atom, :value "sn"}
                 {:pattern/type :atom, :value "hh"}]}]}]}
           (transform-tree (parse-tidal "bd [bd sn hh]" :check-ambiguous? true)))))
  (testing "`:group` [a b] with `x*2`}"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value {:pattern/type :fast
                             :value {:pattern/type :stack
                                     :value [{:pattern/type :fastcat
                                              :len 3
                                              :value
                                              [{:pattern/type :atom, :value "bd"}
                                               {:pattern/type :atom, :value "sn"}
                                               {:pattern/type :atom, :value "hh"}]}]}}
                     :pattern/params [{:pattern/type :atom, :value 2, :value/type :speed}]}]}
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
  (testing "Nested `:group` [a [a , b]] with more than one stack"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :stack
                     :value [{:pattern/type :fastcat
                              :len 2
                              :value [{:pattern/type :atom, :value "bd"}
                                      {:pattern/type :stack
                                       :value [{:pattern/type :fastcat
                                                :len 1
                                                :value [{:pattern/type :atom, :value "sn"}]}
                                               {:pattern/type :fastcat
                                                :len 1
                                                :value [{:pattern/type :atom, :value "hh"}]}]}]}]}]}
           (transform-tree (parse-tidal "bd [bd [sn  , hh]]" :check-ambiguous? true)))))
  (testing "`:slowcat` <a b>"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :slowcat
                     :len 2
                     :value [{:pattern/type :atom, :value "sn"}
                             {:pattern/type :atom, :value "hh"}]}]}
           (transform-tree (parse-tidal "bd <sn hh>" :check-ambiguous? true)))))

  (testing "`:slowcat` stack <a b, c>"
    (is (= {:pattern/type :fastcat
            :len 1
            :value [{:pattern/type :stack
                     :value [{:pattern/type :slowcat
                              :len 2
                              :value [{:pattern/type :atom, :value "sn"}
                                      {:pattern/type :atom, :value "hh"}]}
                             {:pattern/type :slowcat
                              :len 1
                              :value [{:pattern/type :atom, :value "bd"}]}]}]}
           (transform-tree (parse-tidal "<sn hh, bd>" :check-ambiguous? true)))))
  (testing "`:polymeter` {a b c}"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value {:pattern/type :polymeter
                             :value [[{:pattern/type :atom, :value "bd"}
                                      {:pattern/type :atom, :value "hh"}
                                      {:pattern/type :atom, :value "sn"}]]}
                     :pattern/params [{:pattern/type :atom, :value 3, :value/type :len}]}]}
           (transform-tree (parse-tidal "bd {bd hh sn}" :check-ambiguous? true)))))
  (testing "stacked `:polymeter`s {a b c, d e}"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value
                     {:pattern/type :polymeter
                      :value [[{:pattern/type :atom, :value "bd"}
                               {:pattern/type :atom, :value "hh"}
                               {:pattern/type :atom, :value "sn"}]
                              [{:pattern/type :atom, :value "hh"}
                               {:pattern/type :atom, :value "sn"}]]}
                     :pattern/params [{:pattern/type :atom, :value 3, :value/type :len}]}]}
           (transform-tree (parse-tidal "bd {bd hh sn, hh sn}" :check-ambiguous? true)))))
  (testing "`:polymeter` with steps {a b c}%8"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value {:pattern/type :polymeter
                             :value [[{:pattern/type :atom, :value "bd"}
                                      {:pattern/type :atom, :value "hh"}
                                      {:pattern/type :atom, :value "sn"}]]}
                     :pattern/params [{:pattern/type :atom, :value 8, :value/type :len}]}]}
           (transform-tree (parse-tidal "bd {bd hh sn}%8" :check-ambiguous? true)))))
  (testing "`:degrade` a?"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value {:pattern/type :degrade-by
                             :value {:pattern/type :atom, :value "bd"}}
                     :pattern/params [{:pattern/type :atom, :value 0.5, :value/type :probability}]}]}
           (transform-tree (parse-tidal "bd bd?" :check-ambiguous? true)))))
  (testing "`:degrade` with amount a?0.1"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :with-param-pattern
                     :value {:pattern/type :degrade-by
                             :value {:pattern/type :atom, :value "bd"}}
                     :pattern/params [{:pattern/type :atom, :value 1/10, :value/type :probability}]}]}
           (transform-tree (parse-tidal "bd bd?0.1" :check-ambiguous? true)))))
  #_(testing "`:op-elongate`"
      (is (= [{:elongated {:word "bd"}, :size 2} {:word "bd"}]
             (transform-tree (parse-tidal "bd@2 bd" :check-ambiguous? true))))
      (is (= [{:word "sn"} {:elongated {:word "bd"}, :size 2} {:word "hh"}]
             (transform-tree (parse-tidal "sn bd@2 hh" :check-ambiguous? true)))))
  (testing "`:op-sample`"
    (is (= {:pattern/type :fastcat
            :len 1
            :value [{:pattern/type :with-param-pattern
                     :value {:pattern/type :atom, :value "bd"}
                     :pattern/params [{:pattern/type :atom, :value 2, :value/type :n}]}]}
           (transform-tree (parse-tidal "bd:2" :check-ambiguous? true)))))
  (testing "`:op-euclidean`"
    (is (= {:pattern/type :fastcat
            :len 1
            :value [{:pattern/type :with-param-pattern
                     :value {:pattern/type :euclidean
                             :value {:pattern/type :atom, :value "bd"}}
                     :pattern/params [{:pattern/type :atom, :value 3, :value/type :pulses}
                                      {:pattern/type :atom, :value 8, :value/type :steps}
                                      {:pattern/type :atom, :value 0, :value/type :rotation}]}]}
           (transform-tree (parse-tidal "bd(3, 8)" :check-ambiguous? true))))
    (is (=  {:pattern/type :fastcat
             :len 1
             :value [{:pattern/type :with-param-pattern
                      :value {:pattern/type :euclidean
                              :value {:pattern/type :atom, :value "bd"}}
                      :pattern/params [{:pattern/type :atom, :value 3, :value/type :pulses}
                                       {:pattern/type :atom, :value 8, :value/type :steps}
                                       {:pattern/type :atom, :value 1, :value/type :rotation}]}]}
            (transform-tree (parse-tidal "bd(3, 8, 1)" :check-ambiguous? true))))
    (is (=  {:pattern/type :fastcat
             :len 1
             :value [{:pattern/type :with-param-pattern
                      :value {:pattern/type :euclidean
                              :value {:pattern/type :atom, :value "bd"}}
                      :pattern/params [{:pattern/type :slowcat
                                        :len 2
                                        :value [{:pattern/type :atom, :value 3, :value/type :pulses}
                                                {:pattern/type :atom, :value 4, :value/type :pulses}]}
                                       {:pattern/type :slowcat
                                        :len 2
                                        :value [{:pattern/type :atom, :value 8, :value/type :steps}
                                                {:pattern/type :atom, :value 16, :value/type :steps}]}
                                       {:pattern/type :slowcat
                                        :len 3
                                        :value [{:pattern/type :atom, :value 0, :value/type :rotation}
                                                {:pattern/type :atom, :value 1, :value/type :rotation}
                                                {:pattern/type :atom, :value 2, :value/type :rotation}]}]}]}
            (transform-tree (parse-tidal "bd(<3 4>, <8 16>, <0 1 2>)" :check-ambiguous? true))))))
