(ns aoc-2023-day6
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[time-line distance-line] (str/split-lines input)
        parse-numbers #(map parse-long (re-seq #"\d+" %))]
    [(parse-numbers time-line)
     (parse-numbers distance-line)]))

(defn ways-to-win [time record]
  (let [discriminant (Math/sqrt (- (* time time) (* 4 record)))
        lower (/ (- time discriminant) 2)
        upper (/ (+ time discriminant) 2)
        start (long (Math/ceil (+ lower 0.000001)))
        end (long (Math/floor (- upper 0.000001)))]
    (inc (- end start))))

(defn solve-part1 [times distances]
  (reduce * (map ways-to-win times distances)))

(defn solve-part2 [time distance]
  (ways-to-win time distance))

(defn -main []
  (let [input (slurp "input.txt")
        [times distances] (parse-input input)
        part1-result (solve-part1 times distances)
        part2-time (parse-long (apply str times))
        part2-distance (parse-long (apply str distances))
        part2-result (solve-part2 part2-time part2-distance)]
    (println "Part 1:" part1-result)
    (println "Part 2:" part2-result)))

(-main)
