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

(defn transform-tree
  [parse-tree & {:keys [value-type]}]
  (insta-trans/transform
   {:pattern identity
     ;; TODO rename :cat, should be :fastcat
    :fastcat make-fastcat

    :word (fn [x] (make-atom x value-type))
    :sample (fn [& [value [_ n]]]
              (with-param-pattern
                [(deep-assoc-value-type n :n)]
                value))
     ;; TODO These to below should parse into a :pattern/type :atom
    :int (fn [int-str]
           (make-atom (int (edn/read-string int-str)) value-type))
    :float (fn [float-str]
             (make-atom (rationalize (edn/read-string float-str)) value-type))
    :degrade-amount (fn [float-str]
                      (make-atom (rationalize (edn/read-string float-str)) value-type))
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
              [(deep-assoc-value-type speed :speed)]
              {:pattern/type :fast
               :value x}))
    :slow (fn [x [_ speed]]
            (with-param-pattern
              [(deep-assoc-value-type speed :speed)]
              {:pattern/type :slow
               :value x}))
    :replicate (fn [pat [_ times]]
                 {:replicate (repeat times pat)})
    :polymeter (fn [& [stack [_ steps]]]
                 (let [value (->> stack :value (mapv :value))]
                   (with-param-pattern
                     [(if steps (deep-assoc-value-type steps :len)
                          (make-atom (apply max (map count value)) :len))]
                     {:pattern/type :polymeter
                      :value value})))
    :degrade (fn [& [stack [_ amount]]]
               (with-param-pattern
                 [(if amount (deep-assoc-value-type amount :probability)
                      (make-atom 0.5 :probability))]
                 {:pattern/type :degrade-by
                  :value stack}))
    :elongate (fn [& [pat [_ n]]]
                {:elongated pat
                 :size n})
    :euclidean (fn [& [value [_ pulses steps rotation]]]
                 (with-param-pattern
                   [(deep-assoc-value-type pulses :pulses)
                    (deep-assoc-value-type steps :steps)
                    (deep-assoc-value-type
                     (or rotation {:pattern/type :atom :value 0 :value/type :rotation})
                     :rotation)]
                   {:pattern/type :euclidean
                    :value value}))}
   parse-tree))

(defn parse-pattern
  ([pat-str] (parse-pattern pat-str {}))
  ([pat-str transform-opts]
   (transform-tree (parse-tidal pat-str)
                   transform-opts)))

(defn maybe-parse-pattern
  "In case a pattern is a string or a parsed string"
  ([pat] (maybe-parse-pattern pat {}))
  ([pat opts]
   (if (or (string? pat) (number? pat))
     (parse-pattern pat opts)
     pat)))

#_(parse-pattern "[bd hh cp]/2" {:value-type :sound})
#_(parse-pattern "bd(3, 8, 2)" {:value-type :sound})
