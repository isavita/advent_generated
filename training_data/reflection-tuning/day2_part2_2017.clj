(ns day2-corruption-checksum
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(map read-string (str/split % #"\s+")))))

(defn checksum-part1 [rows]
  (reduce + (map #(- (apply max %) (apply min %)) rows)))

(defn find-evenly-divisible [row]
  (first (for [x row
               y row
               :when (and (not= x y) (zero? (mod x y)))]
           (/ x y))))

(defn checksum-part2 [rows]
  (reduce + (map find-evenly-divisible rows)))

(let [input (parse-input "input.txt")
      part1-result (checksum-part1 input)
      part2-result (checksum-part2 input)]
  (println "Part 1 result:" part1-result)
  (println "Part 2 result:" part2-result))
