(ns day15
  (:require [clojure.string :as str]))

(defn parse-ingredient [line]
  (let [[_ name capacity durability flavor texture calories] 
        (re-matches #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)" line)]
    {:name name 
     :capacity (Integer/parseInt capacity)
     :durability (Integer/parseInt durability)
     :flavor (Integer/parseInt flavor)
     :texture (Integer/parseInt texture)
     :calories (Integer/parseInt calories)}))

(defn score [ingredients amounts]
  (let [capacity (apply + (map #(-> %1 :capacity (* %2)) ingredients amounts))
        durability (apply + (map #(-> %1 :durability (* %2)) ingredients amounts))
        flavor (apply + (map #(-> %1 :flavor (* %2)) ingredients amounts))
        texture (apply + (map #(-> %1 :texture (* %2)) ingredients amounts))]
    (if (or (neg? capacity) (neg? durability) (neg? flavor) (neg? texture))
      0
      (* (max 0 capacity) (max 0 durability) (max 0 flavor) (max 0 texture)))))

(defn calories [ingredients amounts]
  (apply + (map #(-> %1 :calories (* %2)) ingredients amounts)))

(defn max-score [ingredients amounts index]
  (if (= index (count ingredients))
    (if (= (apply + amounts) 100)
      (score ingredients amounts)
      0)
    (apply max (for [i (range 101)]
                 (max-score ingredients (assoc amounts index i) (inc index))))))

(defn max-score-500-calories [ingredients amounts index]
  (if (= index (count ingredients))
    (if (and (= (apply + amounts) 100) (= (calories ingredients amounts) 500))
      (score ingredients amounts)
      0)
    (apply max (for [i (range 101)]
                 (max-score-500-calories ingredients (assoc amounts index i) (inc index))))))

(defn -main []
  (let [ingredients (map parse-ingredient (str/split-lines (slurp "input.txt")))]
    (println "Part 1:" (max-score ingredients (vec (repeat (count ingredients) 0)) 0))
    (println "Part 2:" (max-score-500-calories ingredients (vec (repeat (count ingredients) 0)) 0))))

(-main)