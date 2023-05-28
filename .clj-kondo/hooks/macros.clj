(ns hooks.macros
  (:require [clj-kondo.hooks-api :as api]))

(defn def-pattern-transformations [{:keys [node]}]
  (let [[_ patterns-and-args] (:children node)
        children (mapcat :children (:children patterns-and-args))
        defs (vec (set (concat (remove nil? (map :value children))
                               (map :value (remove nil? (mapcat :children children))))))
        new-node (api/list-node
                  (list*
                   (api/token-node 'declare)
                   (map api/token-node defs)))]
    {:node new-node}))

(comment
  (-> {:node (api/parse-string
              "(def-pattern-transformations  [[slow [speed]] [fast [speed]]])")}
      def-pattern-transformations))
