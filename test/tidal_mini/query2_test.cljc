(ns tidal-mini.query2-test
  (:require
   [clojure.test :refer [deftest is]]
   [tidal-mini.query2 :refer [rev-event]]))

(deftest rev-event-test
  (is (= {:value "bd", :arc/active [1/2 1]}
         (rev-event 0 {:value "bd" :arc/active [0 1/2]})))
  (is (= {:value "bd", :arc/active [0 1]}
         (rev-event 0 {:value "bd" :arc/active [0 1]})))
  (is (= {:value :silence, :arc/active [-1N 1N], :original-value "bd"}
         (rev-event 0 {:value "bd" :arc/active [0 2]})))
  (is (= {:value "bd", :arc/active [1 3]}
         (rev-event 1 {:value "bd" :arc/active [0 2]})))
  (is (= {:value :silence, :arc/active [-1/2 1N], :original-value "bd"}
         (rev-event 0 {:value "bd" :arc/active [0 3/2]})))
  (is (= {:value "bd", :arc/active [3/2 3]}
         (rev-event 1 {:value "bd" :arc/active [0 3/2]})))
  (is (= {:value "bd", :arc/active [5/3 7/3]}
         (rev-event 1 {:value "bd" :arc/active [2/3 4/3]}))))

(+ 3/2 (- 3/2 4/3))
(+ 3/2 (- 3/2 2/3))
