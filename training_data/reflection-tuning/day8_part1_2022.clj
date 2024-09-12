(ns treetop-tree-house
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (mapv #(mapv (comp parse-long str) %) input))

(defn visible? [grid x y]
  (let [height (get-in grid [y x])
        row (grid y)
        col (mapv #(nth % x) grid)
        directions [
          (subvec row 0 x)             ; left
          (subvec row (inc x))         ; right
          (subvec col 0 y)             ; up
          (subvec col (inc y))         ; down
        ]]
    (some #(every? (partial > height) %) directions)))

(defn count-visible-trees [grid]
  (let [height (count grid)
        width (count (first grid))
        edge-trees (* 2 (+ height width -2))
        interior-visible (for [y (range 1 (dec height))
                               x (range 1 (dec width))
                               :when (visible? grid x y)]
                           1)]
    (+ edge-trees (count interior-visible))))

(defn solve []
  (let [input (str/split-lines (slurp "input.txt"))
        grid (parse-input input)]
    (println (count-visible-trees grid))))

(solve)
