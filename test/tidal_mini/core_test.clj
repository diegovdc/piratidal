(ns tidal-mini.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.core :refer [parse-tidal-pattern]]))

(deftest parse-tidal-pattern-test
  (testing "Basic pattern"
    (is (= [:pattern [:cat [:word "a"] [:silence "~"] [:word "b"]]]
           (parse-tidal-pattern "a ~ b"))))
  (testing "Basic pattern with `x!2`"
    (is (= [:pattern
            [:cat [:replicate [:word "a"] [:op-replicate [:int "2"]]] [:silence "~"] [:word "b"]]]
           (parse-tidal-pattern "a!2 ~ b"))))
  (testing "`:group` [a b]}"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:group
              [:stack
               [:pattern
                [:cat [:word "bd"] [:word "sn"] [:word "hh"]]]]]]]
           (parse-tidal-pattern "bd [bd sn hh]"))))
  (testing "`:group` [a b] with `x!2`}"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:replicate
              [:group
               [:stack [:pattern [:cat [:word "bd"] [:word "sn"] [:word "hh"]]]]]
              [:op-replicate [:int "2"]]]]]
           (parse-tidal-pattern "bd [bd sn hh]!2"))))
  (testing "`:group` with `:op-choose` [a | b]}"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:choose
              [:stack [:pattern [:cat [:word "bd"]]]]
              [:stack [:pattern [:cat [:word "sn"]]]]
              [:stack [:pattern [:cat [:word "hh"]]]]]]]
           (parse-tidal-pattern "bd [bd | sn | hh]"))))
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
           (parse-tidal-pattern "bd [bd [sn hh]]"))))
  (testing "`:alt` <a b>"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:alt [:stack
                    [:pattern
                     [:cat
                      [:word "sn"] [:word "hh"]]]]]]]
           (parse-tidal-pattern "bd <sn hh>"))))
  (testing "`:alt` <a b> with `x!2`"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:replicate
              [:alt [:stack [:pattern [:cat [:word "sn"] [:word "hh"]]]]]
              [:op-replicate [:int "2"]]]]]
           (parse-tidal-pattern "bd <sn hh>!2"))))
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
           (parse-tidal-pattern "bd <sn <hh bd>>"))))
  (testing "`:polymeter` {a b c}"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:polymeter
              [:stack
               [:pattern
                [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]]]]
           (parse-tidal-pattern "bd {bd hh sn}"))))
  (testing "`:polymeter` {a b c}!2"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:replicate
              [:polymeter
               [:stack [:pattern [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]]
              [:op-replicate [:int "2"]]]]]
           (parse-tidal-pattern "bd {bd hh sn}!2"))))
  (testing "`:polymeter` with steps {a b c}%8"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:polymeter
              [:stack
               [:pattern
                [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]
              [:polymeter-steps [:int "8"]]]]]
           (parse-tidal-pattern "bd {bd hh sn}%8"))))
  (testing "`:polymeter` with steps {a b c}%8!2"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:replicate
              [:polymeter
               [:stack [:pattern [:cat [:word "bd"] [:word "hh"] [:word "sn"]]]]
               [:polymeter-steps [:int "8"]]]
              [:op-replicate [:int "2"]]]]]
           (parse-tidal-pattern "bd {bd hh sn}%8!2"))))
  (testing "`:degrade` a?"
    (is (= [:pattern
            [:cat [:word "bd"] [:degrade [:word "bd"] [:op-degrade]]]]
           (parse-tidal-pattern "bd bd?"))))
  (testing "`:degrade` with amount a?0.1"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:degrade [:word "bd"] [:op-degrade [:float "0.1"]]]]]
           (parse-tidal-pattern "bd bd?0.1"))))
  (testing "`:op-elongate`"
    (is (= [:pattern
            [:cat [:elongate [:word "bd"] [:op-elongate [:int "2"]]] [:word "bd"]]]
           (parse-tidal-pattern "bd@2 bd"))))
  (testing "`:op-sample`"
    (is (= [:pattern
            [:cat [:sample [:word "bd"] [:op-sample [:int "2"]]]]]
           (parse-tidal-pattern "bd:2"))))
  (testing "`:op-euclidean`"
    (is (= [:pattern
            [:cat
             [:euclidean
              [:word "bd"]
              [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
           (parse-tidal-pattern "bd(3, 8, 1)"))))
  (testing "`:op-euclidean` with sample"
    (is (= [:pattern
            [:cat
             [:euclidean
              [:sample [:word "bd"] [:op-sample [:int "2"]]]
              [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]]]
           (parse-tidal-pattern "bd:2(3, 8, 1)"))))
  (testing "`:op-euclidean` + `:op-replicate`"
    (is (= [:pattern
            [:cat
             [:replicate
              [:euclidean
               [:sample [:word "bd"] [:op-sample [:int "2"]]]
               [:op-euclidean [:int "3"] [:int "8"] [:int "1"]]]
              [:op-replicate [:int "2"]]]]]
           (parse-tidal-pattern "bd:2(3, 8, 1)!2"))))
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
           (parse-tidal-pattern "bd:2(3, 8, 1)!2@2"))))
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
           (parse-tidal-pattern
            "a ~  b*2 [c*2 c] <d e> . <a b>!2 a/2 [a , <b , c>]")))))
