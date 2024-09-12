(ns day06.core
  (:require [clojure.string :as str]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #", "))
       (map (fn [coord] (mapv #(Integer/parseInt %) coord)))))

(defn find-bounds [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [(apply min xs) (apply max xs) (apply min ys) (apply max ys)]))

(defn closest-coord [point coords]
  (let [distances (map #(manhattan-distance point %) coords)
        min-dist (apply min distances)]
    (when (= 1 (count (filter #(= min-dist %) distances)))
      (first (keep-indexed #(when (= %2 min-dist) %1) distances)))))

(defn count-area [coords]
  (let [[min-x max-x min-y max-y] (find-bounds coords)
        width (- max-x min-x)
        height (- max-y min-y)]
    (->> (for [x (range (- min-x width) (+ max-x width))
               y (range (- min-y height) (+ max-y height))]
           (closest-coord [x y] coords))
         (frequencies)
         (remove (fn [[k v]]
                   (or (nil? k)
                       (some #(or (= (first %) min-x)
                                  (= (first %) max-x)
                                  (= (second %) min-y)
                                  (= (second %) max-y))
                             (keep-indexed #(when (= %1 k) %2) coords)))))
         (map second)
         (apply max))))

(defn count-safe-region [coords threshold]
  (let [[min-x max-x min-y max-y] (find-bounds coords)]
    (->> (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
           (let [total-distance (reduce + (map #(manhattan-distance [x y] %) coords))]
             (if (< total-distance threshold) 1 0)))
         (reduce +))))

(defn solve [input]
  (let [coords (parse-input input)]
    {:part1 (count-area coords)
     :part2 (count-safe-region coords 10000)}))

;; Example usage:
;; (solve (slurp "input.txt"))
