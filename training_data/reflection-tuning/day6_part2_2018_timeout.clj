(ns day6
  (:require [clojure.string :as str]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn parse-input [input]
  (map #(mapv parse-long (str/split % #", ")) input))

(defn bounding-box [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn closest-coord [point coords]
  (let [distances (map #(manhattan-distance point %) coords)
        min-dist (apply min distances)]
    (if (= 1 (count (filter #(= min-dist %) distances)))
      (first (keep-indexed #(when (= min-dist %2) %1) distances))
      nil)))

(defn solve-part1 [coords]
  (let [[min-x min-y max-x max-y] (bounding-box coords)
        width (- max-x min-x)
        height (- max-y min-y)
        area-sizes (atom (vec (repeat (count coords) 0)))
        infinite-areas (atom #{})]
    (doseq [x (range (dec min-x) (+ max-x 2))
            y (range (dec min-y) (+ max-y 2))]
      (when-let [closest (closest-coord [x y] coords)]
        (swap! area-sizes update closest inc)
        (when (or (= x (dec min-x)) (= x (+ max-x 1))
                  (= y (dec min-y)) (= y (+ max-y 1)))
          (swap! infinite-areas conj closest))))
    (->> (map-indexed vector @area-sizes)
         (remove #(@infinite-areas (first %)))
         (map second)
         (apply max))))

(defn solve-part2 [coords]
  (let [[min-x min-y max-x max-y] (bounding-box coords)
        width (- max-x min-x)
        height (- max-y min-y)]
    (count
     (for [x (range (dec min-x) (+ max-x 2))
           y (range (dec min-y) (+ max-y 2))
           :when (< (reduce + (map #(manhattan-distance [x y] %) coords)) 10000)]
       [x y]))))

(defn -main []
  (let [input (str/split-lines (slurp "input.txt"))
        coords (parse-input input)]
    (println "Part 1:" (solve-part1 coords))
    (println "Part 2:" (solve-part2 coords))))

(-main)
