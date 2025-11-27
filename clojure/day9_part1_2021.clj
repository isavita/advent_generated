
(ns main
  (:require [clojure.string :as str]))

(defn read-grid [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (mapv #(mapv (comp parse-long str) %) (line-seq rdr))))

(defn neighbors [[row col] h w]
  (keep identity
        (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]]
          (let [r (+ row dr) c (+ col dc)]
            (when (and (< -1 r h) (< -1 c w)) [r c])))))

(defn low-points [grid]
  (let [h (count grid) w (count (grid 0))]
    (for [r (range h) c (range w)
          :let [v (get-in grid [r c])]
          :when (every? #(< v %) (map #(get-in grid %) (neighbors [r c] h w)))]
      v)))

(->> "input.txt" read-grid low-points (map inc) (reduce +) println)
