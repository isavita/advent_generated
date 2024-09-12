(ns day8.solve
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (mapv (fn [line] (mapv #(Character/digit % 10) line))
        (str/split-lines input)))

(defn visible? [grid x y]
  (let [height (get-in grid [y x])
        row (nth grid y)
        col (map #(nth % x) grid)
        directions [#(take x (reverse row))
                    #(drop (inc x) row)
                    #(take y (reverse col))
                    #(drop (inc y) col)]]
    (some #(every? (fn [tree] (< tree height)) (%)) directions)))

(defn count-visible-trees [grid]
  (let [height (count grid)
        width (count (first grid))]
    (count (for [y (range height)
                 x (range width)
                 :when (visible? grid x y)]
             [x y]))))

(defn viewing-distance [trees height]
  (let [blocked (take-while #(< % height) trees)]
    (if (= (count blocked) (count trees))
      (count blocked)
      (inc (count blocked)))))

(defn scenic-score [grid x y]
  (let [height (get-in grid [y x])
        row (nth grid y)
        col (map #(nth % x) grid)
        directions [(reverse (take x row))
                    (drop (inc x) row)
                    (reverse (take y col))
                    (drop (inc y) col)]]
    (apply * (map #(viewing-distance % height) directions))))

(defn highest-scenic-score [grid]
  (let [height (count grid)
        width (count (first grid))]
    (apply max (for [y (range height)
                     x (range width)]
                 (scenic-score grid x y)))))

(defn solve [input]
  (let [grid (parse-input input)]
    [(count-visible-trees grid)
     (highest-scenic-score grid)]))

(defn -main [& args]
  (let [[part1 part2] (solve (slurp (first args)))]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))
