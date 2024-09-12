(ns day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-instruction [instruction]
  (let [[_ direction distance] (re-find #"([RULD])(\d+)" instruction)]
    [direction (Integer/parseInt distance)]))

(defn move [[x y] direction]
  (case direction
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn trace-wire [instructions]
  (loop [remaining instructions
         position [0 0]
         steps 0
         points {}]
    (if (empty? remaining)
      points
      (let [[direction distance] (parse-instruction (first remaining))
            new-points (for [i (range 1 (inc distance))]
                         (let [new-pos (move position direction)]
                           [new-pos (+ steps i)]))
            updated-points (into points new-points)
            new-position (last (map first new-points))]
        (recur (rest remaining)
               new-position
               (+ steps distance)
               updated-points)))))

(defn find-intersections [wire1 wire2]
  (let [common-points (set/intersection (set (keys wire1)) (set (keys wire2)))]
    (remove #(= % [0 0]) common-points)))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-part1 [input]
  (let [[wire1 wire2] (str/split-lines input)
        path1 (trace-wire (str/split wire1 #","))
        path2 (trace-wire (str/split wire2 #","))
        intersections (find-intersections path1 path2)]
    (apply min (map manhattan-distance intersections))))

(defn solve-part2 [input]
  (let [[wire1 wire2] (str/split-lines input)
        path1 (trace-wire (str/split wire1 #","))
        path2 (trace-wire (str/split wire2 #","))
        intersections (find-intersections path1 path2)]
    (apply min (map #(+ (get path1 %) (get path2 %)) intersections))))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))
