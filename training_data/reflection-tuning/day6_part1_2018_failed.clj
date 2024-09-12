(ns day6.core
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #", "))
       (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]))
       (into [])))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn find-grid-bounds [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [(apply min xs) (apply max xs) (apply min ys) (apply max ys)]))

(defn closest-coord [point coords]
  (let [distances (map #(manhattan-distance point %) coords)
        min-distance (apply min distances)
        closest (filter #(= min-distance (manhattan-distance point %)) coords)]
    (when (= 1 (count closest))
      (first closest))))

(defn is-infinite? [coord [min-x max-x min-y max-y]]
  (or (= (first coord) min-x)
      (= (first coord) max-x)
      (= (second coord) min-y)
      (= (second coord) max-y)))

(defn largest-finite-area [coords]
  (let [[min-x max-x min-y max-y] (find-grid-bounds coords)
        grid-points (for [x (range (dec min-x) (+ 2 max-x))
                          y (range (dec min-y) (+ 2 max-y))]
                      [x y])
        closest-map (into {} (map (fn [point] [point (closest-coord point coords)]) grid-points))
        infinite-coords (set (filter #(is-infinite? % [min-x max-x min-y max-y]) (vals closest-map)))
        finite-areas (remove #(infinite-coords (second %)) closest-map)]
    (->> finite-areas
         (group-by second)
         (map (fn [[k v]] [k (count v)]))
         (sort-by second >)
         first
         second)))

(defn solve [input]
  (let [coords (parse-input input)]
    (largest-finite-area coords)))
