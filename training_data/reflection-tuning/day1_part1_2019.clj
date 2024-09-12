(ns fuel-calculator
  (:require [clojure.string :as str]))

(defn calculate-fuel [mass]
  (- (int (/ mass 3)) 2))

(defn solve-fuel-requirement []
  (->> (slurp "input.txt")
       (str/split-lines)
       (map #(Integer/parseInt %))
       (map calculate-fuel)
       (reduce +)))

(println (solve-fuel-requirement))
