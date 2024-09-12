(ns toboggan-trajectory
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn count-trees [grid right down]
  (let [width (count (first grid))
        height (count grid)]
    (loop [x 0 y 0 trees 0]
      (if (>= y height)
        trees
        (recur (mod (+ x right) width)
               (+ y down)
               (if (= \# (get-in grid [y x]))
                 (inc trees)
                 trees))))))

(defn solve-part1 [input]
  (let [grid (parse-input input)]
    (count-trees grid 3 1)))

(defn solve-part2 [input]
  (let [grid (parse-input input)
        slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (reduce * (map #(apply count-trees grid %) slopes))))

(defn -main [& args]
  (let [input (slurp (first args))]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))
