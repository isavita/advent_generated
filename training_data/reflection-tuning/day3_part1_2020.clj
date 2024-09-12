(ns toboggan-trajectory
  (:require [clojure.string :as str]))

(defn read-map [file]
  (str/split-lines (slurp file)))

(defn tree? [map-lines x y]
  (= \# (get-in map-lines [y (mod x (count (first map-lines)))])))

(defn count-trees [map-lines right down]
  (loop [x 0, y 0, trees 0]
    (if (>= y (count map-lines))
      trees
      (recur (+ x right)
             (+ y down)
             (if (tree? map-lines x y) (inc trees) trees)))))

(defn solve [file]
  (let [map-lines (read-map file)]
    (count-trees map-lines 3 1)))

(println (solve "input.txt"))
