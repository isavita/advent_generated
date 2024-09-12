(ns day3.core
  (:require [clojure.string :as str]))

(defn symbol? [c]
  (and (not= c \.) (not (Character/isDigit c))))

(defn adjacent-symbol? [schematic row col]
  (let [rows (count schematic)
        cols (count (first schematic))]
    (some symbol?
          (for [r (range (max 0 (dec row)) (min rows (+ row 2)))
                c (range (max 0 (dec col)) (min cols (+ col 2)))
                :when (not= [r c] [row col])]
            (get-in schematic [r c])))))

(defn process-schematic [schematic]
  (loop [row 0
         col 0
         current-num 0
         is-part? false
         sum 0]
    (if (>= row (count schematic))
      sum
      (let [c (get-in schematic [row col])
            [next-row next-col] (if (>= col (dec (count (first schematic))))
                                  [(inc row) 0]
                                  [row (inc col)])]
        (cond
          (Character/isDigit c)
          (recur next-row
                 next-col
                 (+ (* current-num 10) (Character/digit c 10))
                 (or is-part? (adjacent-symbol? schematic row col))
                 sum)

          (and (pos? current-num) is-part?)
          (recur next-row next-col 0 false (+ sum current-num))

          :else
          (recur next-row next-col 0 false sum))))))

(defn solve []
  (let [input (str/split-lines (slurp "input.txt"))
        schematic (mapv vec input)]
    (process-schematic schematic)))

(println (solve))
