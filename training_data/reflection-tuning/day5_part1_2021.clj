(ns hydrothermal-venture
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[x1 y1 x2 y2] (map #(Integer/parseInt %)
                           (re-seq #"\d+" line))]
    [[x1 y1] [x2 y2]]))

(defn horizontal-or-vertical? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn points-on-line [[[x1 y1] [x2 y2]]]
  (let [dx (compare x2 x1)
        dy (compare y2 y1)]
    (for [i (range (inc (max (abs (- x2 x1)) (abs (- y2 y1)))))]
      [(+ x1 (* i dx)) (+ y1 (* i dy))])))

(defn solve-hydrothermal-venture []
  (->> (slurp "input.txt")
       str/split-lines
       (map parse-line)
       (filter horizontal-or-vertical?)
       (mapcat points-on-line)
       frequencies
       (filter #(>= (val %) 2))
       count))

(println (solve-hydrothermal-venture))
