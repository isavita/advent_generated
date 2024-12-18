
(ns gear-ratios
  (:require [clojure.string :as str]))

(defn parse-schematic [lines]
  (let [grid (mapv vec lines)
        rows (count grid)
        cols (count (first grid))]
    {:grid grid :rows rows :cols cols}))

(defn symbol? [ch]
  (and (not (Character/isDigit ch)) (not= ch \.)))

(defn adjacent-to-symbol? [schematic row col]
  (let [{:keys [grid rows cols]} schematic]
    (some (fn [[dr dc]]
            (let [r (+ row dr) c (+ col dc)]
              (and (>= r 0) (< r rows) (>= c 0) (< c cols)
                   (symbol? (get-in grid [r c])))))
          [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))

(defn extract-part-numbers [schematic]
  (let [{:keys [grid rows cols]} schematic
        part-numbers (atom [])]
    (doseq [r (range rows)]
      (loop [c 0 num "" adjacent? false]
        (cond
          (= c cols) (when (and (not (str/blank? num)) adjacent?)
                       (swap! part-numbers conj (parse-long num)))
          (Character/isDigit (get-in grid [r c]))
          (let [new-adjacent? (or adjacent? (adjacent-to-symbol? schematic r c))]
            (recur (inc c) (str num (get-in grid [r c])) new-adjacent?))
          :else
          (do
            (when (and (not (str/blank? num)) adjacent?)
              (swap! part-numbers conj (parse-long num)))
            (recur (inc c) "" false)))))
    @part-numbers))

(defn solve [input-file]
  (let [lines (str/split-lines (slurp input-file))
        schematic (parse-schematic lines)
        part-numbers (extract-part-numbers schematic)]
    (reduce + part-numbers)))

(println (solve "input.txt"))
