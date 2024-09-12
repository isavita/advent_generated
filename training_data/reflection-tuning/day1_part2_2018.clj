(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn part1 [changes]
  (reduce + changes))

(defn part2 [changes]
  (reduce
   (fn [[seen current] freq]
     (if (seen freq)
       (reduced freq)
       [(conj seen freq) freq]))
   [#{0} 0]
   (reductions + (cycle changes))))

(defn solve []
  (let [input (parse-input "input.txt")
        part1-result (part1 input)
        part2-result (part2 input)]
    (println "Part 1:" part1-result)
    (println "Part 2:" part2-result)))

(solve)
