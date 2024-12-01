(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (let [lines (slurp filename)]
    (reduce
      (fn [[left-list right-list] line]
        (let [[left right] (map #(Integer/parseInt %) (str/split line #"\s+"))]
          [(conj left-list left) (conj right-list right)]))
      [[] []]
      (str/split-lines lines))))

(defn total-distance [left-list right-list]
  (let [sorted-left (sort left-list)
        sorted-right (sort right-list)]
    (reduce + (map #(Math/abs (- %1 %2)) sorted-left sorted-right))))

(defn -main []
  (let [[left-list right-list] (parse-input "input.txt")
        distance (total-distance left-list right-list)]
    (println "Total distance:" distance)))

(-main)
