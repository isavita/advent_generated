(ns calorie-counting
  (:require [clojure.string :as str]))

(defn parse-calories [input]
  (->> (str/split input #"\n\n")
       (map #(str/split-lines %))
       (map #(map parse-long %))
       (map #(reduce + %))
       (sort >)))

(defn solve-part1 [calories]
  (first calories))

(defn solve-part2 [calories]
  (reduce + (take 3 calories)))

(let [input (slurp "input.txt")
      calories (parse-calories input)]
  (println "Part 1:" (solve-part1 calories))
  (println "Part 2:" (solve-part2 calories)))
