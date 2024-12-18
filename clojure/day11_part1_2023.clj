
(ns cosmic-expansion
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (str/split-lines (slurp filename)))

(defn find-galaxies [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= \# (get-in grid [y x]))]
    [x y]))

(defn expand-universe [grid]
  (let [empty-rows (set (for [y (range (count grid))
                               :when (every? #(= \. %) (grid y))]
                           y))
        empty-cols (set (for [x (range (count (first grid)))
                               :when (every? #(= \. (get-in grid [% x])) (range (count grid)))]
                           x))
        galaxies (find-galaxies grid)]
    [galaxies empty-rows empty-cols]))

(defn manhattan-distance [[x1 y1] [x2 y2] empty-rows empty-cols expansion-factor]
  (let [min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)
        row-crossings (count (filter #(and (> % min-y) (< % max-y)) empty-rows))
        col-crossings (count (filter #(and (> % min-x) (< % max-x)) empty-cols))]
    (+ (- max-x min-x) (- max-y min-y)
       (* (dec expansion-factor) (+ row-crossings col-crossings)))))

(defn solve [filename expansion-factor]
  (let [[galaxies empty-rows empty-cols] (expand-universe (parse-input filename))]
    (->> (for [i (range (count galaxies))
               j (range (inc i) (count galaxies))]
           (manhattan-distance (nth galaxies i) (nth galaxies j) empty-rows empty-cols expansion-factor))
         (reduce +))))

(println (solve "input.txt" 2))
