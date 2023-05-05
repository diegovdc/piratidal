(ns tidal-mini.parser
  (:require
   [clojure.edn :as edn]
   [instaparse.core :as insta]
   [instaparse.transform :as insta-trans]
   [tidal-mini.euclidean-rhythm :refer [euclidean-rhythm]]
   [tidal-mini.utils :refer [rotate]]))

(def tidal-pattern-grammar
  #?(:clj (slurp "src/tidal_mini/tidal.grammar")
     ;; FIXME cljs needs to get the grammar somehow
     :cljs (throw (ex-info "Implement loading the grammar in JS" {}))))

(defn parse-tidal
  [input & {:keys [check-ambiguous?]
            :or {check-ambiguous? false}}]
  (let [parser (insta/parser tidal-pattern-grammar)
        parsed-data (insta/parse parser input)]
    (when check-ambiguous?
      (let [parses (insta/parses parser input)]
        (when (> (count (seq parses)) 1)
          (throw (ex-info "Grammar is ambiguous"
                          {:parses parses})))))
    (if (insta/failure? parsed-data)
      (println (insta/get-failure parsed-data))
      parsed-data)))

(defn transform-tree
  [parse-tree]
  (insta-trans/transform
   {:pattern identity
    :cat (fn [& xs]
           (reduce
            (fn [acc pat]
              (cond
                (:replicate pat) (into [] (concat acc (:replicate pat)))
                (:euclidean pat) (into [] (concat acc (:euclidean pat)))
                :else (conj acc pat)))
            []
            xs))
    :word (fn [x] {:word x})
    :sample (fn [& [{:keys [word]} [_ n]]]
              {:sample word :n n})
    :int (fn [int-str] (int (edn/read-string int-str)))
    :float (fn [float-str]
             (double (edn/read-string float-str)))
    :degrade-amount (fn [float-str]
                      (double (edn/read-string float-str)))
    :silence (constantly :silence)
    :group vector
    :stack (fn [& xs] {:stack (into [] xs)})
    :alt (fn [& xs] {:alt (into [] xs)})
    :fast (fn [x [_ speed]]
            {:fast x
             :speed speed})
    :slow (fn [x [_ speed]]
            {:slow x
             :speed speed})
    :replicate (fn [pat [_ times]]
                 {:replicate (repeat times pat)})
    :polymeter (fn [& [stack [_ steps]]]
                 {:polymeter stack
                  :steps (or steps (->> stack :stack (map count) (apply max)))})
    :degrade (fn [& [stack [_ amount]]]
               {:degrade stack
                :amount (or amount 0.5)})
    :elongate (fn [& [pat [_ n]]]
                {:elongated pat
                 :size n})
    :euclidean (fn [& [pat [_ pulses steps phase]]]
                 {:euclidean (-> (euclidean-rhythm pulses steps)
                                 (rotate (or phase 0))
                                 (->> (map (fn [play?]
                                             (if-not (zero? play?)
                                               pat :silence)))))})}
   parse-tree))

(def parse-pattern (comp transform-tree parse-tidal))
