
(ns aoc.day13
  (:require [clojure.string :as str]))

(defn transpose [grid]
  (apply mapv vector grid))

(defn find-reflection [grid smudge-diff]
  (let [n (count grid)]
    (first
     (for [i (range 1 n)
           :let [size (min i (- n i))
                 top (subvec grid (- i size) i)
                 bottom (->> (subvec grid i (+ i size)) reverse)]
           :when (= smudge-diff (reduce + (map (fn [r1 r2] (count (filter (fn [[a b]] (not= a b)) (map vector r1 r2)))) top bottom)))]
       i))))

(defn solve [input smudge-diff]
  (let [grids (->> (str/split input #"\n\n")
                   (map #(str/split-lines %))
                   (map #(mapv vec %)))]
    (reduce + (for [grid grids
                    :let [horizontal (find-reflection grid smudge-diff)
                          vertical (find-reflection (transpose grid) smudge-diff)]]
                (cond
                  horizontal (* 100 horizontal)
                  vertical vertical
                  :else 0)))))

(let [input (slurp "input.txt")]
  (println "Part 1:" (solve input 0))
  (println "Part 2:" (solve input 1)))
