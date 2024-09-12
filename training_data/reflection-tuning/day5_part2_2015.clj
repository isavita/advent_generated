(ns day5
  (:require [clojure.string :as str]))

(defn nice-string-part1? [s]
  (and (>= (count (re-seq #"[aeiou]" s)) 3)
       (re-find #"(.)\1" s)
       (not (re-find #"ab|cd|pq|xy" s))))

(defn nice-string-part2? [s]
  (and (re-find #"(..).*\1" s)
       (re-find #"(.).\1" s)))

(defn count-nice-strings [pred strings]
  (count (filter pred strings)))

(let [input (str/split-lines (slurp "input.txt"))
      part1-count (count-nice-strings nice-string-part1? input)
      part2-count (count-nice-strings nice-string-part2? input)]
  (println "Part 1:" part1-count)
  (println "Part 2:" part2-count))
