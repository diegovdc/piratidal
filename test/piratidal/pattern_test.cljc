(ns tidal-mini.pattern-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tidal-mini.query2
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
                [0 2])))
  (testing "a single cycle"
    (is (= [{:value/type :sound :value "cp", :arc/whole [4/3 2N], :arc/active [4/3 2N]}]
           (query {:pattern/type :slow
                   :speed 2
                   :value {:pattern/type :fastcat
                           :len 3
                           :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                   {:pattern/type :atom :value/type :sound :value "hh"}
                                   {:pattern/type :atom :value/type :sound :value "cp"}]}}
                  [1 2])))))

(deftest speed-test
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1/6], :arc/active [0 1/6]}
          {:value/type :sound :value "hh", :arc/whole [1/6 1/3], :arc/active [1/6 1/3]}
          {:value/type :sound :value "hh", :arc/whole [1/3 2/3], :arc/active [1/3 2/3]}
          {:value/type :sound :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}]
         (query {:pattern/type :speed
                 :value {:pattern/type :fastcat
                         :len 3
                         :value [{:pattern/type :atom :value 2}
                                 {:pattern/type :atom :value 1}
                                 {:pattern/type :atom :value 1}]}
                 :speedable-pattern {:pattern/type :fast
                                     :value {:pattern/type :fastcat
                                             :len 3
                                             :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                                     {:pattern/type :atom :value/type :sound :value "hh"}
                                                     {:pattern/type :atom :value/type :sound :value "cp"}]}}}
                [0 1]))))

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
    (is (= [{:value/type :sound :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}
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
    (is (= [{:value/type :sound :value "cp", :arc/whole [2/3 1], :arc/active [2/3 1]}
            {:value/type :sound :value "bd", :arc/whole [1 4/3], :arc/active [1 4/3]}]
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
  (is (= [{:value/type :sound :value "bd", :arc/whole [0N 2N], :arc/active [0N 1N]}
          ;; TODO figure out if active should span more than one cycle
          {:value/type :sound :value "bd", :arc/whole [1N 3N], :arc/active [3N 5N]}]
         (remove-silences
          (query {:pattern/type :palindrome
                  :value {:pattern/type :slow
                          :speed 2
                          :value {:pattern/type :atom :value/type :sound :value "bd"}}}
                 [0 4]))))
  (is (= [{:value/type :sound :value "bd", :arc/whole [0 1/3], :arc/active [0 1/3]}
          {:value/type :sound :value "cp", :arc/whole [1/3 2/3], :arc/active [2/3 1N]}
          {:value/type :sound :value "hh", :arc/whole [2/3 1], :arc/active [4/3 5/3]}
          {:value/type :sound :value "hh", :arc/whole [0N 1/3], :arc/active [1N 4/3]}
          {:value/type :sound :value "cp", :arc/whole [1/3 2/3], :arc/active [5/3 2N]}
          {:value/type :sound :value "bd", :arc/whole [2/3 1N], :arc/active [7/3 8/3]}
          {:value/type :sound :value "bd", :arc/whole [1 4/3], :arc/active [2 7/3]}
          {:value/type :sound :value "cp", :arc/whole [4/3 5/3], :arc/active [8/3 3N]}
          {:value/type :sound :value "hh", :arc/whole [5/3 2], :arc/active [10/3 11/3]}]
         (remove-silences
          (query {:pattern/type :palindrome
                  :value {:pattern/type :fastcat
                          :len 3
                          :value [{:pattern/type :atom :value/type :sound :value "bd"}
                                  {:pattern/type :atom :value/type :sound :value "cp"}
                                  {:pattern/type :atom :value/type :sound :value "hh"}]}}
                 [0 3])))))
