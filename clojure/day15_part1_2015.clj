
(ns day15
  (:require [clojure.string :as str]))

(defn parse-ingredient [line]
  (let [[_ name cap dur flav text cal] (re-matches #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)" line)]
    {:name name
     :capacity (parse-long cap)
     :durability (parse-long dur)
     :flavor (parse-long flav)
     :texture (parse-long text)
     :calories (parse-long cal)}))

(defn calculate-score [ingredients amounts]
  (let [capacity (max 0 (reduce + (map #(* (:capacity %1) %2) ingredients amounts)))
        durability (max 0 (reduce + (map #(* (:durability %1) %2) ingredients amounts)))
        flavor (max 0 (reduce + (map #(* (:flavor %1) %2) ingredients amounts)))
        texture (max 0 (reduce + (map #(* (:texture %1) %2) ingredients amounts)))]
    (* capacity durability flavor texture)))


(defn solve [ingredients total-teaspoons]
    (let [num-ingredients (count ingredients)]
      (letfn [(find-best-score [current-amounts remaining-teaspoons]
                (if (= (count current-amounts) (- num-ingredients 1))
                    (let [last-amount remaining-teaspoons
                          amounts (conj current-amounts last-amount)]
                        (calculate-score ingredients amounts))
                  (reduce max (for [i (range (inc remaining-teaspoons))]
                                  (find-best-score (conj current-amounts i) (- remaining-teaspoons i))))))]
    (find-best-score [] total-teaspoons))))
  

(defn -main []
  (let [input-lines (str/split-lines (slurp "input.txt"))
        ingredients (map parse-ingredient input-lines)]
    (println (solve ingredients 100))))

(-main)
