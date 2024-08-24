(ns solution
  (:require [clojure.string :as str]))

(defn is-small-cave [cave]
  (re-matches #"[a-z]+" cave))

(defn count-paths [graph current visited]
  (if (= current "end")
    1
    (let [visited (if (is-small-cave current) (conj visited current) visited)]
      (reduce + (for [next (get graph current [])
                      :when (not (contains? visited next))]
                  (count-paths graph next visited))))))

(defn main []
  (let [input (slurp "input.txt")
        edges (map #(str/split % #"-") (str/split-lines input))
        graph (reduce (fn [g [a b]]
                        (-> g
                            (update a conj b)
                            (update b conj a)))
                      {}
                      edges)]
    (println (count-paths graph "start" #{}))))

(main)