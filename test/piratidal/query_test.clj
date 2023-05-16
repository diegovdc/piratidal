(ns piratidal.query-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [piratidal.query
    :refer [extend-arc query take-slow-segment translate-arc]]
   [piratidal.parser :refer [parse-pattern]]))

(deftest translate-arc-test
  (is (= [1/4 1/2]
         (translate-arc 1/2 [0 1/2] [3/4 1]))))

(deftest extend-arc-test
  (is (= {:value {:word "a"}, :arc [3/2 9/4]}
         (extend-arc 3 {:value {:word "a"}, :arc [1/2 3/4]}))))

(deftest take-slow-segment-test
  (is (= [{:value {:word "a"}, :arc [1/2 3/4], :cycle 1}
          {:value {:word "b"}, :arc [3/4 1N], :cycle 1}]
         (take-slow-segment
          {:speed 1
           :cycle 1
           :elapsed-arc 1/2
           :end-arc 1}
          [{:value {:word "a"}
            :arc [1/2 3/4]}
           {:value {:word "b"}
            :arc [3/4 1N]}]))))

(deftest query-test
  (let [pat->query (fn [pattern cycles]
                     (into []
                           (mapcat (fn [cycle] (->> pattern
                                                    parse-pattern
                                                    (query {:index 0 :elapsed-arc 0 :cycle cycle})))
                                   cycles)))
        query->word-str #(->> %
                              (map (comp :word :value))
                              (str/join " "))
        remove-commas #(str/replace % "," "")]
    (testing ""
      (is (= [{:value {:word "bd"}, :arc [0 1/3], :cycle 0}
              {:value :silence, :arc [1/3 2/3], :cycle 0}
              {:value {:word "bd"}, :arc [2/3 1N], :cycle 0}]
             (pat->query "bd ~ bd" [0])))
      (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 0}
              {:value {:word "bd"}, :arc [1/2 3/4], :cycle 0}
              {:value {:word "sn"}, :arc [3/4 1N], :cycle 0}
              {:value {:word "hh"}, :arc [3/4 7/8], :cycle 0}
              {:value {:word "bd"}, :arc [7/8 1N], :cycle 0}]
             (pat->query "bd [bd [sn , [hh bd]]]" [0])))
      (is (= [{:value {:word "hh"}, :arc [0 1], :cycle 0}
              {:value {:word "sn"}, :arc [0 1], :cycle 1}]
             (pat->query "<hh sn>" [0 1])))

      (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 0}
              {:value {:word "hh"}, :arc [1/2 1N], :cycle 0}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 1}
              {:value {:word "sn"}, :arc [1/2 1N], :cycle 1}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 2}
              {:value {:word "hh"}, :arc [1/2 1N], :cycle 2}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 3}
              {:value {:word "sn"}, :arc [1/2 1N], :cycle 3}]
             (pat->query "bd <hh sn>" [0 1 2 3])))
      (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 3}
              {:value {:word "tom"}, :arc [1/2 1N], :cycle 3}]
             (pat->query "bd <hh <sn tom>>" [3])))
      (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 0}
              {:value {:word "hh"}, :arc [1/2 1N], :cycle 0}
              {:value {:word "tom"}, :arc [1/2 1N], :cycle 0}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 1}
              {:value {:word "bd"}, :arc [1/2 1N], :cycle 1}
              {:value {:word "sn"}, :arc [1/2 1N], :cycle 1}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 2}
              {:value {:word "hh"}, :arc [1/2 1N], :cycle 2}
              {:value {:word "bd"}, :arc [1/2 1N], :cycle 2}]
             (pat->query "bd <hh bd, tom sn bd>" (range 3))))
      (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 0}
              {:value {:word "sn"}, :arc [1/2 1N], :cycle 0}
              {:value {:word "tom"}, :arc [1/2 3/4], :cycle 0}
              {:value {:word "sn"}, :arc [3/4 1N], :cycle 0}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 1}
              {:value {:word "hh"}, :arc [1/2 1N], :cycle 1}
              {:value {:word "sd"}, :arc [1/2 1N], :cycle 1}
              {:value {:word "bd"}, :arc [0 1/2], :cycle 2}
              {:value {:word "sn"}, :arc [1/2 1N], :cycle 2}
              {:value {:word "tom"}, :arc [1/2 3/4], :cycle 2}
              {:value {:word "sn"}, :arc [3/4 1N], :cycle 2}]
             (pat->query "bd <sn hh, [tom sn] sd>" (range 3))))
      (testing "`:polymeter`"
        (is (= [{:value {:word "hh"}, :arc [0 1/3], :cycle 0}
                {:value {:word "sn"}, :arc [1/3 2/3], :cycle 0}
                {:value {:word "sd"}, :arc [2/3 1N], :cycle 0}]
               (pat->query "{hh sn sd}" (range 1))))
        (is (= [{:value {:word "bd"}, :arc [0 1/2], :cycle 0}
                {:value {:word "hh"}, :arc [1/2 2/3], :cycle 0}
                {:value {:word "sn"}, :arc [2/3 5/6], :cycle 0}
                {:value {:word "sd"}, :arc [5/6 1N], :cycle 0}]
               (pat->query "bd {hh sn sd}" (range 1))))
        (is (= [{:value {:word "a"}, :arc [0 1/4], :cycle 0}
                {:value {:word "b"}, :arc [1/4 1/2], :cycle 0}
                {:value {:word "c"}, :arc [1/2 3/4], :cycle 0}
                {:value {:word "a"}, :arc [3/4 1N], :cycle 0}
                {:value {:word "b"}, :arc [0 1/4], :cycle 1}
                {:value {:word "c"}, :arc [1/4 1/2], :cycle 1}
                {:value {:word "a"}, :arc [1/2 3/4], :cycle 1}
                {:value {:word "b"}, :arc [3/4 1N], :cycle 1}]
               (pat->query "{a b c}%4" (range 2))))
        (is (= [{:value {:word "a"}, :arc [0 1/4], :cycle 0}
                {:value {:word "b"}, :arc [1/4 1/2], :cycle 0}
                {:value {:word "c"}, :arc [1/2 3/4], :cycle 0}
                {:value {:word "a"}, :arc [3/4 1N], :cycle 0}
                {:value {:word "b"}, :arc [0 1/4], :cycle 1}
                {:value {:word "d"}, :arc [1/4 1/2], :cycle 1}
                {:value {:word "a"}, :arc [1/2 3/4], :cycle 1}
                {:value {:word "b"}, :arc [3/4 1N], :cycle 1}
                {:value {:word "c"}, :arc [0 1/4], :cycle 2}
                {:value {:word "a"}, :arc [1/4 1/2], :cycle 2}
                {:value {:word "b"}, :arc [1/2 3/4], :cycle 2}
                {:value {:word "d"}, :arc [3/4 1N], :cycle 3}]
               (pat->query "{a b <c d>}%4" (range 3))))
        (let [pat "{bd <hh cp crash>}%3"]
          (is (= "bd hh bd cp bd crash"
                 (->> (pat->query pat (range 2))
                      query->word-str))))
        (let [pat "[{bd <hh cp crash>}%3]!2"]
          (is (= "bd hh bd bd hh bd cp bd crash cp bd crash"
                 (->> (pat->query pat (range 2))
                      query->word-str))))
        (let [pat "[{bd <hh cp crash>}%3]*2"]
          (is (= "bd hh bd cp bd crash bd hh bd cp bd crash"
                 (->> (pat->query pat (range 2))
                      query->word-str)))))
      (testing "`:euclidean`"
        (is (= [{:value {:word "hh"}, :arc [0 1/8], :cycle 0}
                {:value :silence, :arc [1/8 1/4], :cycle 0}
                {:value :silence, :arc [1/4 3/8], :cycle 0}
                {:value {:word "hh"}, :arc [3/8 1/2], :cycle 0}
                {:value :silence, :arc [1/2 5/8], :cycle 0}
                {:value :silence, :arc [5/8 3/4], :cycle 0}
                {:value {:word "hh"}, :arc [3/4 7/8], :cycle 0}
                {:value :silence, :arc [7/8 1N], :cycle 0}]
               (pat->query "hh(3, 8)" [0]))))
      (testing "`:replicate`"
        (is (= [{:value {:word "hh"}, :arc [0 1/3], :cycle 0}
                {:value {:word "hh"}, :arc [1/3 2/3], :cycle 0}
                {:value {:word "bd"}, :arc [2/3 1N], :cycle 0}]
               (pat->query "hh!2 bd" [0]))))
      (testing "`:elongate`"
        (is (= [{:value {:word "hh"}, :arc [0 2/3], :cycle 0}
                {:value {:word "bd"}, :arc [2/3 1N], :cycle 0}]
               (pat->query "hh@2 bd" [0])))
        (is (= [{:value {:word "hh"}, :arc [0 1/3], :cycle 0}
                {:value {:word "sn"}, :arc [1/3 2/3], :cycle 0}
                {:value {:word "bd"}, :arc [2/3 1N], :cycle 0}]
               (pat->query "[hh sn]@2 bd" [0])))
        (is (= [{:value {:word "bd"}, :arc [0 2/9], :cycle 0}
                {:value {:word "hh"}, :arc [2/9 4/9], :cycle 0}
                {:value {:word "sn"}, :arc [4/9 2/3], :cycle 0}
                {:value {:word "bd"}, :arc [2/3 1N], :cycle 0}]
               (pat->query "[bd hh sn]@2 bd" [0]))))
      (testing "`:slow`"
        ;; TODO more tests are needed here
        (is (= [{:value {:word "a"}, :arc [0 2], :cycle 0}]
               (pat->query "a/2" (range 2))))
        (is (= [{:value {:word "a"}, :arc [0 1N], :cycle 0}
                {:value {:word "b"}, :arc [0N 1N], :cycle 1}]
               (pat->query "[a b]/2" (range 2))))
        (is (= [{:value {:word "a"}, :arc [0 1N], :cycle 0}
                {:value {:word "b"}, :arc [0N 1N], :cycle 1}
                {:value {:word "a"}, :arc [0 1N], :cycle 2}]
               (pat->query "[a b]/2" (range 3))))
        (is (= [{:value {:word "a"}, :arc [0 1/2], :cycle 0}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 0}
                {:value {:word "b"}, :arc [0N 1/2], :cycle 1}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 1}
                {:value {:word "a"}, :arc [0 1/2], :cycle 2}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 2}]
               (pat->query "[a b]/2 c" (range 3))))
        (is (= [{:value {:word "a"}, :arc [0 3/4], :cycle 0}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 0}
                {:value {:word "b"}, :arc [1/4 1N], :cycle 1}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 1}
                {:value {:word "c"}, :arc [1/2 1N], :cycle 2}]
               (pat->query "[a b]/3 c" (range 3))))
        (is (= [{:value {:word "c"}, :arc [0 1/2], :cycle 0}
                {:value {:word "a"}, :arc [1/2 5/4], :cycle 0}
                {:value {:word "c"}, :arc [0 1/2], :cycle 1}
                {:value {:word "b"}, :arc [3/4 3/2], :cycle 1}
                {:value {:word "c"}, :arc [0 1/2], :cycle 2}
                {:value {:word "c"}, :arc [0 1/2], :cycle 3}
                {:value {:word "a"}, :arc [1/2 5/4], :cycle 3}
                {:value {:word "c"}, :arc [0 1/2], :cycle 4}
                {:value {:word "b"}, :arc [3/4 3/2], :cycle 4}
                {:value {:word "c"}, :arc [0 1/2], :cycle 5}]
               (pat->query "c [a b]/3" (range 6))))
        (is (= (remove-commas "a b a c a, a b a d a") ; cycles separated by commas
               (query->word-str (pat->query "a [b <c d>]/3" (range 6)))))
        (is (= (remove-commas "a b a c a, a b a d a, a b a e a")
               (query->word-str (pat->query "a [b <c d e>]/3" (range 9)))))
        (is (= [{:value {:word "a"}, :arc [0 1/2], :cycle 0}
                {:value {:word "b"}, :arc [1/2 5/4], :cycle 0}
                {:value {:word "a"}, :arc [0 1/2], :cycle 1}
                {:value {:word "c"}, :arc [3/4 3/2], :cycle 1}
                {:value {:word "a"}, :arc [0 1/2], :cycle 2}
                {:value {:word "a"}, :arc [0 1/2], :cycle 3}
                {:value {:word "b"}, :arc [1/2 5/4], :cycle 3}
                {:value {:word "a"}, :arc [0 1/2], :cycle 4}
                {:value {:word "d"}, :arc [3/4 3/2], :cycle 4}
                {:value {:word "a"}, :arc [0 1/2], :cycle 5}
                {:value {:word "a"}, :arc [0 1/2], :cycle 6}
                {:value {:word "b"}, :arc [1/2 5/4], :cycle 6}
                {:value {:word "a"}, :arc [0 1/2], :cycle 7}
                {:value {:word "e"}, :arc [3/4 9/8], :cycle 7}
                {:value {:word "a"}, :arc [0 1/2], :cycle 8}
                {:value {:word "f"}, :arc [5/8 1N], :cycle 8}
                {:value {:word "a"}, :arc [0 1/2], :cycle 9}
                {:value {:word "b"}, :arc [1/2 5/4], :cycle 9}]
               (pat->query "a [b <c d [e f]>]/3" (range 10))))
        (is (= "a b a c a a b a d a a b a e a a b a f a"
               (query->word-str (pat->query "a [b <<c f> d e>]/3" (range 12))))))
      (testing "`:fast`"
        (is (= [{:value {:word "a"}, :arc [0N 1/2], :cycle 0}
                {:value {:word "a"}, :arc [1/2 1N], :cycle 0}]
               (pat->query "a*2" [0])))
        (is (= [{:value {:word "a"}, :arc [0N 1/4], :cycle 0}
                {:value {:word "a"}, :arc [1/4 1/2], :cycle 0}
                {:value {:word "b"}, :arc [1/2 1N], :cycle 0}]
               (pat->query "a*2 b" [0])))
        (is (= [{:value {:word "a"}, :arc [0N 1/4], :cycle 0}
                {:value {:word "c"}, :arc [1/4 1/2], :cycle 0}
                {:value {:word "b"}, :arc [1/2 1N], :cycle 0}
                {:value {:word "a"}, :arc [0N 1/4], :cycle 1}
                {:value {:word "c"}, :arc [1/4 1/2], :cycle 1}
                {:value {:word "b"}, :arc [1/2 1N], :cycle 1}]
               (pat->query "<a c>*2 b" (range 2))))
        (is (= [{:value {:word "a"}, :arc [0N 1/8], :cycle 0}
                {:value {:word "c"}, :arc [1/8 1/4], :cycle 0}
                {:value {:word "a"}, :arc [1/4 3/8], :cycle 0}
                {:value {:word "c"}, :arc [3/8 1/2], :cycle 0}
                {:value {:word "b"}, :arc [1/2 1N], :cycle 0}
                {:value {:word "a"}, :arc [0N 1/8], :cycle 1}
                {:value {:word "c"}, :arc [1/8 1/4], :cycle 1}
                {:value {:word "a"}, :arc [1/4 3/8], :cycle 1}
                {:value {:word "c"}, :arc [3/8 1/2], :cycle 1}
                {:value {:word "b"}, :arc [1/2 1N], :cycle 1}]
               (pat->query "[a c]*2 b" (range 2))))
        (is (= [{:value {:word "hh"}, :arc [0N 1/6], :cycle 0}
                {:value {:word "cp"}, :arc [1/6 1/3], :cycle 0}
                {:value {:word "hh"}, :arc [1/3 1/2], :cycle 0}
                {:value {:word "bd"}, :arc [1/2 1N], :cycle 0}
                {:value {:word "cp"}, :arc [0N 1/6], :cycle 1}
                {:value {:word "hh"}, :arc [1/6 1/3], :cycle 1}
                {:value {:word "cp"}, :arc [1/3 1/2], :cycle 1}
                {:value {:word "bd"}, :arc [1/2 1N], :cycle 1}]
               (pat->query "<hh cp>*3 bd" (range 2)))))
      (testing "`:degrade`"
        ;; TODO
        )
      (testing "`:choose`"
        ;; TODO
        )
      (testing "Numeric patterns"
        (testing "integers"
          (is (= [{:value 1, :arc [0 1/3], :cycle 0}
                  {:value 2, :arc [1/3 2/3], :cycle 0}
                  {:value 3, :arc [2/3 1N], :cycle 0}]
                 (pat->query "1 2 3" [0]))))
        (testing "floats"
          (is (= [{:value 1, :arc [0 1/3], :cycle 0}
                  {:value 0.2, :arc [1/3 2/3], :cycle 0}
                  {:value 3, :arc [2/3 1N], :cycle 0}]
                 (pat->query "1 0.2 3" [0]))))
        (testing "`:slowcat`"
          (is (= [{:value 1, :arc [0 1/3], :cycle 0}
                  {:value 0.2, :arc [1/3 2/3], :cycle 0}
                  {:value 3, :arc [2/3 1N], :cycle 0}
                  {:value 1, :arc [0 1/3], :cycle 1}
                  {:value 0.7, :arc [1/3 2/3], :cycle 1}
                  {:value 3, :arc [2/3 1N], :cycle 1}]
                 (pat->query "1 <0.2 0.7> 3" (range 2)))))))))
