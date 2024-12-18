
(ns ceres-search
  (:require [clojure.string :as str]))

(defn find-xmas [grid]
  (let [rows (count grid)
        cols (count (first grid))
        xmas "XMAS"]
    (->> (for [r (range rows)
               c (range cols)
               dr [-1 0 1]
               dc [-1 0 1]
               :when (not (and (= dr 0) (= dc 0)))]
           (loop [i 0
                  current-r r
                  current-c c
                  found-word ""]
             (if (or (>= current-r rows)
                     (< current-r 0)
                     (>= current-c cols)
                     (< current-c 0))
               found-word
               (let [next-found-word (str found-word (get-in grid [current-r current-c]))]
                 (if (= (count next-found-word) (count xmas))
                   next-found-word
                   (recur (inc i) (+ current-r dr) (+ current-c dc) next-found-word))))))
         (filter #(= % xmas))
         count)))

(defn solve []
  (let [grid (->> (slurp "input.txt")
                  str/split-lines
                  (mapv vec))]
    (println (find-xmas grid))))

(solve)
