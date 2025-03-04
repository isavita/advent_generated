
(ns day9
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(vec (map (comp read-string str) (seq %))))))

(defn get-neighbors [grid row col]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
          :let [nr (+ row dr)
                nc (+ col dc)]
          :when (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))]
      [nr nc])))

(defn is-low-point? [grid row col]
  (let [val (get-in grid [row col])]
    (every? #(< val (get-in grid %)) (get-neighbors grid row col))))

(defn find-low-points [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows)
          c (range cols)
          :when (is-low-point? grid r c)]
      [r c])))

(defn part1 [grid]
  (->> (find-low-points grid)
       (map #(inc (get-in grid %)))
       (reduce +)))

(defn basin-size [grid low-point]
  (loop [visited #{}
         q (conj clojure.lang.PersistentQueue/EMPTY low-point)]
    (if (empty? q)
      (count visited)
      (let [[r c :as current] (peek q)
            next-q (pop q)]
        (if (or (visited current) (= 9 (get-in grid current -1)))
          (recur visited next-q)
          (let [neighbors (get-neighbors grid r c)
                unvisited-neighbors (filter #(not (or (visited %) (= 9 (get-in grid % -1)) )) neighbors)]
            (recur (conj visited current) (reduce conj next-q unvisited-neighbors))))))))
            

(defn part2 [grid]
  (->> (find-low-points grid)
       (map #(basin-size grid %))
       (sort >)
       (take 3)
       (reduce *)))

(defn -main []
  (let [input (slurp "input.txt")
        grid (parse-input input)]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
