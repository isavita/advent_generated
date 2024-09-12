(ns advent-of-code.day2
  (:require [clojure.string :as str]))

(defn parse-dimensions [line]
  (map #(Integer/parseInt %) (str/split line #"x")))

(defn calculate-paper [l w h]
  (let [sides [(* l w) (* w h) (* h l)]
        surface-area (* 2 (apply + sides))
        smallest-side (apply min sides)]
    (+ surface-area smallest-side)))

(defn total-paper-needed [input]
  (->> input
       str/split-lines
       (map parse-dimensions)
       (map #(apply calculate-paper %))
       (reduce +)))

(defn solve [input]
  (total-paper-needed input))

;; Example usage:
;; (solve "2x3x4\n1x1x10")
