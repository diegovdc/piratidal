(ns examples
  (:refer-clojure :exclude [+ - / * mod quot loop])
  (:require
   [piratidal.core :refer :all]
   [piratidal.harmony :refer [set-scales!]]))

(comment
  (p 1 (-> (s "[bd cp/2 hh]")
           (jux rev)))

  (p 1 (-> (sound "[bd*8, <~ hh(3, 8)>]")
           (n "3 4")
           (jux rev)
           (+ n "<1 4>")
           (note "1 3 0")
           (+ note "<3 5 5 3 1>")))

  (p 1 (-> (sound "[bd*8, <~ hh(3, 8)>]")
           (n "3 4")
           (+ n "<1 4>")
           (note "1 3 0")
           (jux palindrome)
           (+ note "<3 5 5 3 1>")))

  (p 1 (-> (sound "[<bd sd hh:7> cp/2 <hh [bd bd bd:8]?>]*2")
           (gain "{1 0.8 0.7}%4")
           (end "<1 [0.01 0.5, 0.1 0.7] 0.6>")
           (jux #(-> % (n "<0 0 0 2 3 4 7 8>") (rotr "<0 0 0 0.2>") (gain "0.8 1 0.6")))
           palindrome
           (jux #(-> % (speed "{1 2 3}") (crush "8 3 5") (gain "0.5 1?")))
           (sometimes-by 0.5 #(-> % (s "kurt")))
           (sometimes-by 0.5 #(-> % (note "1 [3 10]")))
           (sometimes-by 0.1 #(-> % (s "can")))
           (off "0.333 0.5" #(-> % (crush "[2 3 4, 16]")))
           (jux #(speed % "<1 2> <2 0.75 1.5>"))))

  ;; scale2
  (set-scales!
   {"hex1" {:ratios [1 15/14 5/4 10/7 3/2 12/7]
            :period 2}
    "hex2" {:ratios [1 9/7 5/3 15/7]
            :period 3}})

  (p 1 (-> (note "0 1 2 3 4 5 6")
           (+ note "<0 6>")
           (sound "arpy")
           (scale2 "hex1")))

  (hush))
