(ns day3.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn char-priority [c]
  (let [ascii (int c)]
    (if (>= ascii (int \a))
      (- ascii 96)  ; lowercase
      (- ascii 38)))) ; uppercase

(defn find-common-item [rucksack]
  (let [half-size (/ (count rucksack) 2)
        first-half (set (take half-size rucksack))
        second-half (set (drop half-size rucksack))]
    (first (set/intersection first-half second-half))))

(defn solve-part1 [input]
  (->> input
       str/split-lines
       (map find-common-item)
       (map char-priority)
       (reduce +)))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))))
