(ns day7
  (:require [clojure.string :as str]))

(defn fuel-cost [n]
  (/ (* n (inc n)) 2))

(defn total-fuel [positions target]
  (apply + (map #(fuel-cost (Math/abs (- % target))) positions)))

(defn -main []
  (let [positions (map #(Integer/parseInt %) (str/split (slurp "input.txt") #","))]
    (let [min-fuel (apply min (map #(total-fuel positions %) (range (apply min positions) (inc (apply max positions)))))
          target (first (filter #(= (total-fuel positions %) min-fuel) (range (apply min positions) (inc (apply max positions)))))]
      (println "The crabs must spend" min-fuel "fuel to align to position" target))))

(-main)