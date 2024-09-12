(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/split-lines)
       (mapv #(Integer/parseInt %))))

(defn count-increases [coll]
  (->> (partition 2 1 coll)
       (filter (fn [[a b]] (> b a)))
       count))

(defn part1 [depths]
  (count-increases depths))

(defn part2 [depths]
  (->> (partition 3 1 depths)
       (map #(apply + %))
       count-increases))

(defn solve []
  (let [depths (parse-input "input.txt")]
    (println "Part 1:" (part1 depths))
    (println "Part 2:" (part2 depths))))

(solve)
