(ns piratidal.parser
  (:require
   [clojure.edn :as edn]
   [instaparse.core :as insta]
   [instaparse.transform :as insta-trans]))

(def tidal-pattern-grammar
  #?(:clj (slurp "src/piratidal/tidal.grammar")
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

(defn make-fastcat
  [& xs]
  {:pattern/type :fastcat
   ;; ::tag tag
   :len (count xs) ;; FIXME this must be more sofisticated
   :value (into [] xs)})

(defn make-slowcat [xs]
  {:pattern/type :slowcat
   :len (count xs) ;; TODO improve, need sofistication
   :value xs})

(defn make-atom [x]
  {:pattern/type :atom :value x})

(defn transform-tree
  [parse-tree]
  (insta-trans/transform
   {:pattern identity
    ;; TODO rename :cat, should be :fastcat
    :fastcat make-fastcat

    :word make-atom
    :sample (fn [& [value [_ n]]]
              (assoc value :n n))
    ;; TODO These to below should parse into a :pattern/type :atom
    :int (fn [int-str] (int (edn/read-string int-str)))
    :float (fn [float-str]
             (double (edn/read-string float-str)))
    :degrade-amount (fn [float-str]
                      (double (edn/read-string float-str)))
    :silence (fn [_] {:pattern/type :atom :value :silence})
    :group identity
    :stack (fn [& xs] {:pattern/type :stack :value (into [] xs)})
    :slowcat (fn [& xs]
               (if (= 1 (count xs))
                 (make-slowcat (first xs))
                 {:pattern/type :stack
                  :value (mapv make-slowcat xs)}))
    :slowcat-token (fn [& xs] (vec xs))
    :fast (fn [x [_ speed]]
            {:pattern/type :fast
             :value x
             :speed speed})
    :slow (fn [x [_ speed]]
            {:pattern/type :slow
             :value x
             :speed speed})
    :replicate (fn [pat [_ times]]
                 {:replicate (repeat times pat)})
    :polymeter (fn [& [stack [_ steps]]]
                 (let [value (->> stack :value (mapv :value))]
                   {:pattern/type :polymeter
                    :value value
                    :len (or steps (apply max (map count value)))}))
    :degrade (fn [& [stack [_ amount]]]
               {:pattern/type :degrade-by
                :value stack
                :probability (or amount 0.5)})
    :elongate (fn [& [pat [_ n]]]
                {:elongated pat
                 :size n})
    :euclidean (fn [& [value [_ pulses steps rotation]]]
                 {:pattern/type :euclidean
                  :value value
                  :pulses pulses
                  :steps steps
                  :rotation (or rotation 0)})}
   parse-tree))

(def parse-pattern (comp transform-tree parse-tidal))
