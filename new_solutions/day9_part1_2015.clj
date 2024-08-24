(ns shortest-route
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (reduce (fn [acc line]
              (let [[_ from to distance] (re-matches #"(.*) to (.*) = (\d+)" line)]
                (-> acc
                    (update-in [from to] #(if % (+ % (Integer/parseInt distance)) (Integer/parseInt distance)))
                    (update-in [to from] #(if % (+ % (Integer/parseInt distance)) (Integer/parseInt distance))))))
            {} lines)))

(defn all-permutations [coll]
  (if (empty? coll)
    '(())
    (for [x coll
          xs (all-permutations (remove #{x} coll))]
      (cons x xs))))

(defn calculate-distance [route distances]
  (reduce + (map #(get-in distances [%1 %2]) route (rest route))))

(defn shortest-route [distances]
  (let [locations (keys distances)
        routes (all-permutations locations)]
    (apply min (map #(calculate-distance % distances) routes))))

(defn -main []
  (let [input (slurp "input.txt")
        distances (parse-input input)]
    (println (shortest-route distances))))

(-main)