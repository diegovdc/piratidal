(ns piratidal.parser
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [instaparse.transform :as insta-trans]
   [piratidal.utils :refer [deep-assoc-value-type]]))

(def tidal-pattern-grammar
  #?(:clj (slurp "src/piratidal/tidal.grammar")
     ;; FIXME cljs needs to get the grammar somehow
     :cljs (throw (ex-info "Implement loading the grammar in JS" {}))))

(defn parse-tidal
  [input & {:keys [check-ambiguous?]
            :or {check-ambiguous? false}}]
  (let [input (str/trim (if (string? input) input (str input)))
        parser (insta/parser tidal-pattern-grammar)
        parsed-data (insta/parse parser input)]
    (when check-ambiguous?
      (let [parses (insta/parses parser input)]
        (when (> (count (seq parses)) 1)
          (throw (ex-info "Grammar is ambiguous"
                          {:parses parses})))))
    (if (insta/failure? parsed-data)
      (println (insta/get-failure parsed-data))
      parsed-data)))

(defn make-cat-pattern [xs]
  (reduce (fn [{:keys [len value]} pat]
            {:len (+ len (:pattern/ratio pat 1))
             :value (assoc value len pat)})
          {:len 0 :value {}}
          xs))

(defn make-fastcat
  [& xs]
  (let [value (make-cat-pattern xs)]
    (merge {:pattern/type :fastcat}
           value)))

(defn make-slowcat [xs]
  (merge {:pattern/type :slowcat}
         (make-cat-pattern xs)))

(defn make-atom [x value-type]
  (cond-> {:pattern/type :atom :value x}
    value-type (assoc :value/type value-type)))

#?(:cljs
   ;; TODO implement
   (defn rationalize [x]
     x))

(defn with-param-pattern
  [params pattern]
  {:pattern/type :with-param-pattern
   :value pattern
   :pattern/params params})

(defn validate-operator-value
  [pred error-msg atom-pat]
  (when-not (pred (:value atom-pat))
    (throw (ex-info error-msg {}))))

(do
  (defn transform-tree
    ;; TODO validate that patterned op only take ints and floats, but ensure that patterned sounds to take words
    ;; TODO replicate and elongate still need work
    [parse-tree & {:keys [value-type]}]
    (insta-trans/transform
     {:pattern identity
      :fastcat make-fastcat
      :word (fn [x] (make-atom x value-type))
      :sample (fn [& [value [_ n]]]
                (with-param-pattern
                  [(deep-assoc-value-type n :n)]
                  value))
      :int (fn [int-str]
             (make-atom (int (edn/read-string int-str)) value-type))
      :float (fn [float-str]
               (make-atom (rationalize (edn/read-string float-str)) value-type))
      :degrade-amount identity
      :silence (fn [_] (make-atom :silence value-type))
      :group identity
      :stack (fn [& xs] {:pattern/type :stack :value (into [] xs)})
      :slowcat (fn [& xs]
                 (if (= 1 (count xs))
                   (make-slowcat (first xs))
                   {:pattern/type :stack
                    :value (mapv make-slowcat xs)}))
      :slowcat-token (fn [& xs] (vec xs))
      :fast (fn [x [_ speed]]
              (with-param-pattern
                [(deep-assoc-value-type speed :speed
                                        :validate-value-fn (partial validate-operator-value
                                                                    number?
                                                                    "Fast (*) value must be a number"))]
                {:pattern/type :fast
                 :value x}))
      :slow (fn [x [_ speed]]
              (with-param-pattern
                [(deep-assoc-value-type speed :speed
                                        :validate-value-fn (partial validate-operator-value
                                                                    number?
                                                                    "Slow (*) value must be a number"))]
                {:pattern/type :slow
                 :value x}))
      :replicate (fn [pat [_ times]]
                   {:replicate (repeat (:value times) pat)})
      :polymeter (fn [& [stack [_ steps]]]
                   (let [value (->> stack :value (mapv :value))]
                     (with-param-pattern
                       [(if steps (deep-assoc-value-type steps :len
                                                         :validate-value-fn (partial validate-operator-value
                                                                                     int?
                                                                                     "Polymeter (?) value must be an integer"))
                            (make-atom (apply max (map count value)) :len))]
                       {:pattern/type :polymeter
                        :value value})))
      :degrade (fn [& [stack [_ amount]]]
                 (with-param-pattern
                   [(if amount (deep-assoc-value-type amount :probability
                                                      :validate-value-fn (partial validate-operator-value
                                                                                  number?
                                                                                  "Degrade (?) value must be a number"))
                        (make-atom 0.5 :probability))]
                   {:pattern/type :degrade-by
                    :value stack}))
      :elongate (fn [& [pat [_ ratio]]]
                  (assoc pat :pattern/ratio (:value ratio)))
      :euclidean (fn [& [value [_ pulses steps rotation]]]
                   ;; TODO extract validation to function
                   (with-param-pattern
                     [(deep-assoc-value-type pulses :pulses
                                             :validate-value-fn (partial validate-operator-value
                                                                         int?
                                                                         "Euclidean values must be integers"))
                      (deep-assoc-value-type steps :steps
                                             :validate-value-fn (partial validate-operator-value
                                                                         int?
                                                                         "Euclidean values must be integers"))
                      (deep-assoc-value-type
                       (or rotation {:pattern/type :atom :value 0 :value/type :rotation})
                       :rotation
                       :validate-value-fn (partial validate-operator-value
                                                   int?
                                                   "Euclidean values must be integers"))]
                     {:pattern/type :euclidean
                      :value value}))}
     parse-tree))
  (transform-tree (parse-tidal "bd bd?0.1" :check-ambiguous? true)))

(defn parse-pattern
  ([pat-str] (parse-pattern pat-str {}))
  ([pat-str transform-opts]
   (transform-tree (parse-tidal pat-str)
                   transform-opts)))

(defn maybe-parse-pattern
  "In case a pattern is a string or a parsed string"
  ([pat] (maybe-parse-pattern pat {}))
  ([pat opts]
   (cond
     (string? pat) (parse-pattern pat opts)
     ;; cast to double so that the parser doesn't think it's a slow operator
     (number? pat) (parse-pattern (double pat) opts)
     :else pat)))
