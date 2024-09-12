(ns crab-alignment
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/trim)
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn fuel-cost [crabs target]
  (reduce + (map #(Math/abs (- % target)) crabs)))

(defn min-fuel-cost [crabs]
  (let [min-pos (apply min crabs)
        max-pos (apply max crabs)]
    (apply min
           (for [pos (range min-pos (inc max-pos))]
             (fuel-cost crabs pos)))))

(defn -main []
  (let [crabs (parse-input "input.txt")
        result (min-fuel-cost crabs)]
    (println result)))

(-main)
