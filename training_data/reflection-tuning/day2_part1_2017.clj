(ns day2-checksum
  (:require [clojure.string :as str]))

(defn row-checksum [row]
  (let [nums (map #(Integer/parseInt %) (str/split row #"\s+"))
        max-val (apply max nums)
        min-val (apply min nums)]
    (- max-val min-val)))

(defn calculate-checksum [input]
  (->> input
       str/split-lines
       (map row-checksum)
       (reduce +)))

(defn -main []
  (let [input (slurp "input.txt")
        checksum (calculate-checksum input)]
    (println checksum)))

(-main)
