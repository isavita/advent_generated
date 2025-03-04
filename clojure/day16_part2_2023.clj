
(ns day16
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn energize [grid start-pos start-dir]
  (let [rows (count grid)
        cols (count (first grid))
        in-bounds? (fn [[row col]] (and (>= row 0) (< row rows) (>= col 0) (< col cols)))]
    (loop [beams (list {:pos start-pos :dir start-dir})
           visited #{}]
      (if (empty? beams)
        (count (distinct (map :pos visited)))
        (let [{:keys [pos dir]} (first beams)
              [row col] pos
              [dr dc] dir
              next-beams (rest beams)]
          (if (or (not (in-bounds? pos)) (contains? visited {:pos pos :dir dir}))
            (recur next-beams visited)
            (let [tile (get-in grid [row col])
                  new-dirs (case tile
                             \. [dir]
                             \/ (case dir
                                  [0 1] [[-1 0]]
                                  [0 -1] [[1 0]]
                                  [1 0] [[0 -1]]
                                  [-1 0] [[0 1]])
                             \\ (case dir
                                  [0 1] [[1 0]]
                                  [0 -1] [[-1 0]]
                                  [1 0] [[0 1]]
                                  [-1 0] [[0 -1]])
                             \| (if (or (= dir [0 1]) (= dir [0 -1]))
                                  [[-1 0] [1 0]]
                                  [dir])
                             \- (if (or (= dir [1 0]) (= dir [-1 0]))
                                  [[0 -1] [0 1]]
                                  [dir]))
                  new-beams-conj (map (fn [new-dir] {:pos [(+ row (first new-dir)) (+ col (second new-dir))] :dir new-dir}) new-dirs)]
              (recur (concat next-beams new-beams-conj) (conj visited {:pos pos :dir dir})))))))))

(defn part1 [grid]
  (energize grid [0 0] [0 1]))

(defn part2 [grid]
  (let [rows (count grid)
        cols (count (first grid))
        top-starts (map #(vector [0 %] [1 0]) (range cols))
        bottom-starts (map #(vector [(- rows 1) %] [-1 0]) (range cols))
        left-starts (map #(vector [% 0] [0 1]) (range rows))
        right-starts (map #(vector [% (- cols 1)] [0 -1]) (range rows))]
    (apply max (pmap #(energize grid (first %) (second %)) (concat top-starts bottom-starts left-starts right-starts)))))


(defn -main []
  (let [input (slurp "input.txt")
        grid (parse-input input)]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
