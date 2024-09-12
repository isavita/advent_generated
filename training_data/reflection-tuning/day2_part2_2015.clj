(ns day2.core
  (:require [clojure.string :as str]))

(defn parse-dimensions [line]
  (map #(Integer/parseInt %) (str/split line #"x")))

(defn wrapping-paper [dims]
  (let [[l w h] dims
        sides [(* l w) (* w h) (* h l)]
        surface-area (* 2 (reduce + sides))
        smallest-side (apply min sides)]
    (+ surface-area smallest-side)))

(defn ribbon [dims]
  (let [[l w h] dims
        perimeters [(+ l l w w) (+ w w h h) (+ h h l l)]
        smallest-perimeter (apply min perimeters)
        volume (* l w h)]
    (+ smallest-perimeter volume)))

(defn solve-puzzle []
  (let [dimensions (map parse-dimensions (str/split-lines (slurp "input.txt")))
        total-paper (reduce + (map wrapping-paper dimensions))
        total-ribbon (reduce + (map ribbon dimensions))]
    (println "Total square feet of wrapping paper:" total-paper)
    (println "Total feet of ribbon:" total-ribbon)))

(solve-puzzle)
