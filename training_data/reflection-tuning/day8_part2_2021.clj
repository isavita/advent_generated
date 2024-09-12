(ns day8-solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[patterns output] (str/split line #" \| ")]
    [(str/split patterns #" ") (str/split output #" ")]))

(defn count-unique-segments [outputs]
  (count (filter #(contains? #{2 3 4 7} (count %)) outputs)))

(defn deduce-digits [patterns]
  (let [by-length (group-by count patterns)
        one (first (by-length 2))
        four (first (by-length 4))
        seven (first (by-length 3))
        eight (first (by-length 7))
        zero-six-nine (by-length 6)
        two-three-five (by-length 5)
        bd (set/difference (set four) (set one))
        eg (set/difference (set eight) (set four) (set seven))
        nine (first (filter #(= 1 (count (set/intersection (set %) eg))) zero-six-nine))
        zero (first (filter #(and (not= % nine) (set/superset? (set %) (set one))) zero-six-nine))
        six (first (filter #(and (not= % nine) (not= % zero)) zero-six-nine))
        five (first (filter #(set/subset? bd (set %)) two-three-five))
        three (first (filter #(and (not= % five) (set/subset? (set one) (set %))) two-three-five))
        two (first (filter #(and (not= % five) (not= % three)) two-three-five))]
    (zipmap (map set [zero one two three four five six seven eight nine]) (range 10))))

(defn decode-output [digit-map output]
  (reduce #(+ (* 10 %1) %2) (map #(get digit-map (set %)) output)))

(defn solve-part1 [inputs]
  (reduce + (map (comp count-unique-segments second) inputs)))

(defn solve-part2 [inputs]
  (reduce + (map (fn [[patterns output]]
                   (let [digit-map (deduce-digits patterns)]
                     (decode-output digit-map output)))
                 inputs)))

(defn -main []
  (let [inputs (map parse-line (str/split-lines (slurp "input.txt")))]
    (println "Part 1:" (solve-part1 inputs))
    (println "Part 2:" (solve-part2 inputs))))

(-main)
