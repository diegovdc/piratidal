(ns tidal-mini.core
  (:require [instaparse.core :as insta]))

(do

  (def tidal-pattern-grammar
    (slurp "src/tidal_mini/grammar.txt"))

  (defn parse-tidal-pattern [input]
    (let [parser (insta/parser tidal-pattern-grammar)]
      (insta/parses  parser input)))

  ;; FIXME error that requires a space before the first `a` for `cat` to work
  #_(let [input " a b*2 [c*2 c] <d e> . <a b>!2 a/2 [a , <b , c>]"]
      (parse-tidal-pattern input))
  (let [input "c a .  b"]
    (parse-tidal-pattern input)))
