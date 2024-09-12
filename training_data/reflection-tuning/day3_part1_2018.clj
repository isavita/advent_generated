(ns day3
  (:require [clojure.string :as str]))

(defn parse-claim [line]
  (let [[_ id left top width height] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)]
    (mapv #(Integer/parseInt %) [id left top width height])))

(defn mark-claim [grid [_ left top width height]]
  (reduce (fn [g [x y]]
            (update-in g [y x] (fnil inc 0)))
          grid
          (for [x (range left (+ left width))
                y (range top (+ top height))]
            [x y])))

(defn solve-overlap []
  (let [claims (map parse-claim (str/split-lines (slurp "input.txt")))
        max-x (apply max (map #(+ (nth % 1) (nth % 3)) claims))
        max-y (apply max (map #(+ (nth % 2) (nth % 4)) claims))
        grid (vec (repeat (inc max-y) (vec (repeat (inc max-x) 0))))
        final-grid (reduce mark-claim grid claims)]
    (->> final-grid
         (apply concat)
         (filter #(> % 1))
         count)))

(println (solve-overlap))
