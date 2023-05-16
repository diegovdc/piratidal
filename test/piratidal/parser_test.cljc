(ns piratidal.parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [piratidal.parser :refer [parse-tidal transform-tree]]))

(deftest parse-tidal-test
  (testing
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
    (testing "Complex pattern"
      (is (= [:pattern
              [:fastcat
               [:word "a"]
               [:silence "~"]
               [:fast [:word "b"] [:op-fast [:int "2"]]]
               [:group
                [:stack
                 [:pattern
                  [:fastcat
                   [:fast [:word "c"] [:op-fast [:int "2"]]]
                   [:word "c"]]]]]
               [:slowcat [:slowcat-token [:word "d"] [:word "e"]]]]
              [:fastcat
               [:replicate
                [:slowcat [:slowcat-token [:word "a"] [:word "b"]]]
                [:op-replicate [:int "2"]]]
               [:slow [:word "a"] [:op-slow [:int "2"]]]
               [:group
                [:stack
                 [:pattern [:fastcat [:word "a"]]]
                 [:pattern
                  [:fastcat
                   [:slowcat
                    [:slowcat-token [:word "b"]]
                    [:slowcat-token [:word "c"]]]]]]]]]
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
            :value [1 {:pattern/type :atom, :value :silence} 2]}
           (transform-tree (parse-tidal "1 ~ 2" :check-ambiguous? true)))))

  (testing "Basic pattern with `x*2`"
    (is (= {:pattern/type :fastcat
            :len 3
            :value [{:pattern/type :fast
                     :value {:pattern/type :atom, :value "a"}
                     :speed 2}
                    {:pattern/type :atom, :value :silence}
                    {:pattern/type :atom, :value "b"}]}
           (transform-tree (parse-tidal "a*2 ~ b" :check-ambiguous? true)))))
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
            :value
            [{:pattern/type :atom, :value "bd"}
             {:pattern/type :fast
              :value
              {:pattern/type :stack
               :value
               [{:pattern/type :fastcat
                 :len 3
                 :value
                 [{:pattern/type :atom, :value "bd"}
                  {:pattern/type :atom, :value "sn"}
                  {:pattern/type :atom, :value "hh"}]}]}
              :speed 2}]}
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
                    {:pattern/type :polymeter
                     :value [[{:pattern/type :atom, :value "bd"}
                              {:pattern/type :atom, :value "hh"}
                              {:pattern/type :atom, :value "sn"}]]
                     :len 3}]}
           (transform-tree (parse-tidal "bd {bd hh sn}" :check-ambiguous? true)))))
  (testing "stacked `:polymeter`s {a b c, d e}"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :polymeter
                     :value [[{:pattern/type :atom, :value "bd"}
                              {:pattern/type :atom, :value "hh"}
                              {:pattern/type :atom, :value "sn"}]
                             [{:pattern/type :atom, :value "hh"}
                              {:pattern/type :atom, :value "sn"}]]
                     :len 3}]}
           (transform-tree (parse-tidal "bd {bd hh sn, hh sn}" :check-ambiguous? true)))))
  (testing "`:polymeter` with steps {a b c}%8"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :polymeter
                     :value [[{:pattern/type :atom, :value "bd"}
                              {:pattern/type :atom, :value "hh"}
                              {:pattern/type :atom, :value "sn"}]]
                     :len 8}]}
           (transform-tree (parse-tidal "bd {bd hh sn}%8" :check-ambiguous? true)))))
  (testing "`:degrade` a?"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :degrade-by
                     :value {:pattern/type :atom, :value "bd"}
                     :probability 0.5}]}
           (transform-tree (parse-tidal "bd bd?" :check-ambiguous? true)))))
  (testing "`:degrade` with amount a?0.1"
    (is (= {:pattern/type :fastcat
            :len 2
            :value [{:pattern/type :atom, :value "bd"}
                    {:pattern/type :degrade-by
                     :value {:pattern/type :atom, :value "bd"}
                     :probability 0.1}]}
           (transform-tree (parse-tidal "bd bd?0.1" :check-ambiguous? true)))))
  #_(testing "`:op-elongate`"
      (is (= [{:elongated {:word "bd"}, :size 2} {:word "bd"}]
             (transform-tree (parse-tidal "bd@2 bd" :check-ambiguous? true))))
      (is (= [{:word "sn"} {:elongated {:word "bd"}, :size 2} {:word "hh"}]
             (transform-tree (parse-tidal "sn bd@2 hh" :check-ambiguous? true)))))
  (testing "`:op-sample`"
    (is (= {:pattern/type :fastcat
            :len 1
            :value [{:pattern/type :atom, :value "bd", :n 2}]}
           (transform-tree (parse-tidal "bd:2" :check-ambiguous? true)))))
  (testing "`:op-euclidean`"
    (is (= {:pattern/type :fastcat
            :len 1
            :value [{:pattern/type :euclidean
                     :value {:pattern/type :atom, :value "bd"}
                     :pulses 3
                     :steps 8
                     :rotation 0}]}
           (transform-tree (parse-tidal "bd(3, 8)" :check-ambiguous? true))))
    (is (= {:pattern/type :fastcat
            :len 1
            :value
            [{:pattern/type :euclidean
              :value {:pattern/type :atom, :value "bd"}
              :pulses 3
              :steps 8
              :rotation 1}]}
           (transform-tree (parse-tidal "bd(3, 8, 1)" :check-ambiguous? true))))))
