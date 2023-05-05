(ns tidal-mini.control-patterns-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.control-patterns :refer [gain note palindrome rev]]
   [tidal-mini.parser :refer [parse-pattern]]
   [tidal-mini.schedule :refer [make-schedule]]))

(defn pat->schedule2
  [pattern cycles]
  (into [] (mapcat (fn [cycle]
                     (->> pattern
                          (make-schedule {:index 0 :elapsed-arc 0 :cycle cycle})))
                   cycles)))

(deftest gain-test
  (testing
   (is (= [{:event {:word "a"}, :arc [0 1/3], :cycle 0, :gain 1}
           {:event {:word "b"}, :arc [1/3 2/3], :cycle 0, :gain 2}
           {:event {:word "c"}, :arc [2/3 1N], :cycle 0, :gain 3}]
          (-> (parse-pattern "a b c")
              (gain "1 2 3")
              (pat->schedule2 (range 1)))))
    (is (= [{:event {:word "bd"}, :arc [0 1/3], :cycle 0, :gain 1}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0, :gain 2}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 0, :gain 3}
            {:event {:word "bd"}, :arc [0 1/3], :cycle 1, :gain 1}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1, :gain 1.5}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 1, :gain 3}]
           (-> (parse-pattern "bd hh sn")
               (gain "1 <2 1.5> 3")
               (pat->schedule2 (range 2)))))
    (is (= [{:event {:word "bd"}, :arc [0 1/3], :cycle 0, :gain 1}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0, :gain 1}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 0, :gain 2}
            {:event {:word "bd"}, :arc [0 1/3], :cycle 1, :gain 1}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1, :gain 1}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 1, :gain 2}]
           (-> (parse-pattern "bd hh sn")
               (gain "1 2")
               (pat->schedule2 (range 2)))))
    (is (= [{:event {:word "bd"}, :arc [0 2/3], :cycle 0, :gain 1}
            {:event {:word "hh"}, :arc [2/3 4/3], :cycle 0, :gain 2}
            {:event {:word "sn"}, :arc [1/3 1N], :cycle 1, :gain 1}]
           (-> (parse-pattern "[bd hh sn]/2")
               (gain "1 2")
               (pat->schedule2 (range 2)))))))

(deftest note-test
  (testing
   (is (= [{:event {:word "bd"}, :arc [0 2/3], :cycle 0, :note 1}
           {:event {:word "hh"}, :arc [2/3 4/3], :cycle 0, :note 2}
           {:event {:word "sn"}, :arc [1/3 1N], :cycle 1, :note 1}]
          (-> (parse-pattern "[bd hh sn]/2")
              (note "1 2")
              (pat->schedule2 (range 2))))))
  (testing "gain+note"
    (is (= [{:event {:word "bd"}, :arc [0 1/3], :cycle 0, :gain 1, :note 12}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0, :gain 2, :note 12}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 0, :gain 3, :note 13}
            {:event {:word "bd"}, :arc [0 1/3], :cycle 1, :gain 1, :note 12}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1, :gain 2, :note 12}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 1, :gain 3, :note 13}]
           (-> (parse-pattern "bd hh sn")
               (gain "1 2 3")
               (note "12 13")
               (pat->schedule2 (range 2)))))))

(deftest rev-test
  (testing
   (is (= [{:event {:word "sn"}, :arc [0N 1/3], :cycle 0, :gain 1, :note 12}
           {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0, :gain 2, :note 12}
           {:event {:word "bd"}, :arc [2/3 1], :cycle 0, :gain 3, :note 13}
           {:event {:word "sn"}, :arc [0N 1/3], :cycle 1, :gain 1, :note 12}
           {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1, :gain 2, :note 12}
           {:event {:word "bd"}, :arc [2/3 1], :cycle 1, :gain 3, :note 13}]
          (-> (parse-pattern "bd hh sn")
              rev
              (gain "1 2 3")
              (note "12 13")
              (pat->schedule2 (range 2)))))
    (is (= [{:event {:word "bd"}, :arc [0 2/3], :cycle 0}
            {:event {:word "hh"}, :arc [2/3 4/3], :cycle 0}
            {:event {:word "sn"}, :arc [1/3 1N], :cycle 1}
            {:event {:word "bd"}, :arc [0 2/3], :cycle 2}
            {:event {:word "hh"}, :arc [2/3 4/3], :cycle 2}
            {:event {:word "sn"}, :arc [1/3 1N], :cycle 3}]
           (-> (parse-pattern "[bd hh sn]/2")
               (pat->schedule2 (range 4)))))))

(deftest palindrome-test
  (testing
   (is (= [{:event {:word "bd"}, :arc [0 1/3], :cycle 0}
           {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0}
           {:event {:word "sn"}, :arc [2/3 1N], :cycle 0}
           {:event {:word "sn"}, :arc [0 1/3], :cycle 1}
           {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1}
           {:event {:word "bd"}, :arc [2/3 1N], :cycle 1}]
          (-> (parse-pattern "bd hh sn")
              palindrome
              (pat->schedule2 (range 1)))))
    (is (= [{:event {:word "bd"}, :arc [0 1/3], :cycle 0}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 0}
            {:event {:word "sn"}, :arc [2/3 1N], :cycle 0}
            {:event {:word "sn"}, :arc [0 1/3], :cycle 1}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 1}
            {:event {:word "bd"}, :arc [2/3 1N], :cycle 1}
            {:event {:word "bd"}, :arc [0 1/3], :cycle 2}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 2}
            {:event {:word "cp"}, :arc [2/3 1N], :cycle 2}
            {:event {:word "cp"}, :arc [0 1/3], :cycle 3}
            {:event {:word "hh"}, :arc [1/3 2/3], :cycle 3}
            {:event {:word "bd"}, :arc [2/3 1N], :cycle 3}]
           (-> (parse-pattern "[bd hh <sn cp>]")
               palindrome
               (pat->schedule2 (range 2)))))
    (is (= [{:event {:word "bd"}, :arc [0 2], :cycle 0}
            {:event {:word "bd"}, :arc [0 2], :cycle 3}
            {:event {:word "bd"}, :arc [0 2], :cycle 4}
            {:event {:word "bd"}, :arc [0 2], :cycle 7}]
           (-> (parse-pattern "[bd]/2")
               palindrome
               (pat->schedule2 (range 4)))))
    (is (= [{:event {:word "bd"}, :arc [0 1N], :cycle 0}
            {:event {:word "bd"}, :arc [0 1N], :cycle 1}
            {:event {:word "hh"}, :arc [0N 1N], :cycle 2}
            {:event {:word "hh"}, :arc [0 1N], :cycle 3}
            {:event {:word "sd"}, :arc [0N 1N], :cycle 4}
            {:event {:word "sd"}, :arc [0 1N], :cycle 5}]
           (-> (parse-pattern "[bd hh sd]/3")
               palindrome
               (pat->schedule2 (range 3)))))
    (is (= [{:event {:word "bd"}, :arc [0 1N], :cycle 0}
            {:event {:word "bd"}, :arc [0 1N], :cycle 1}
            {:event {:word "hh"}, :arc [0N 1N], :cycle 2}
            {:event {:word "hh"}, :arc [0 1N], :cycle 3}
            {:event {:word "sd"}, :arc [0N 1N], :cycle 4}
            {:event {:word "sd"}, :arc [0 1N], :cycle 5}]
           (-> (parse-pattern "[bd ~ ~]/2")
               palindrome
               (pat->schedule2 (range 3)))))
    #_(is (= [{:event {:word "bd"}, :arc [0 1/6], :cycle 0}
              {:event {:word "hh"}, :arc [1/6 1/3], :cycle 0}
              {:event {:word "sn"}, :arc [1/3 1/2], :cycle 0}
              {:event {:word "sn"}, :arc [1/2 2/3], :cycle 0}
              {:event {:word "hh"}, :arc [2/3 5/6], :cycle 0}
              {:event {:word "bd"}, :arc [5/6 1N], :cycle 0}
              {:event {:word "bd"}, :arc [0 1/6], :cycle 1}
              {:event {:word "hh"}, :arc [1/6 1/3], :cycle 1}
              {:event {:word "cp"}, :arc [1/3 1/2], :cycle 1}
              {:event {:word "cp"}, :arc [1/2 2/3], :cycle 1}
              {:event {:word "hh"}, :arc [2/3 5/6], :cycle 1}
              {:event {:word "bd"}, :arc [5/6 1N], :cycle 1}]
             (-> (parse-pattern "[bd hh <sd cp>]/2")
                 palindrome
                 () (pat->schedule2 (range 4)))))))
