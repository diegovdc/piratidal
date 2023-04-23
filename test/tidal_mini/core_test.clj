(ns tidal-mini.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.core :refer [parse-tidal-pattern]]))

(deftest parse-tidal-pattern-test
  (testing "Basic pattern"
    (is (= [:pattern [:cat [:word "a"] [:silence "~"] [:word "b"]]]
           (parse-tidal-pattern "a ~ b"))))
  (testing "`:group`"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:group
              [:stack
               [:pattern
                [:cat [:word "bd"] [:word "sn"] [:word "hh"]]]]]]]
           (parse-tidal-pattern "bd [bd sn hh]"))))
  (testing "Nested `:group`"
    (is (= [:pattern
            [:cat
             [:word "bd"]
             [:group
              [:stack
               [:pattern
                [:cat
                 [:word "bd"]
                 [:group
                  [:stack
                   [:pattern
                    [:cat [:word "sn"] [:word "hh"]]]]]]]]]]]
           (parse-tidal-pattern "bd [bd [sn hh]]"))))
  (testing "Complex pattern"
    (is (= [:pattern
            [:cat
             [:word "a"]
             [:silence "~"]
             [:fast [:word "b"] [:number "2"]]
             [:group
              [:stack
               [:pattern [:cat [:fast [:word "c"] [:number "2"]] [:word "c"]]]]]
             [:alt [:stack [:pattern [:cat [:word "d"] [:word "e"]]]]]]
            [:cat
             [:group
              [:alt [:stack [:pattern [:cat [:word "a"] [:word "b"]]]]]
              [:number "2"]]
             [:slow [:word "a"] [:number "2"]]
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
