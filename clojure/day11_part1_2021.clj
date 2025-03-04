
(ns day11
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (fn [line] (mapv #(Character/digit % 10) line)))))

(defn neighbors [grid r c]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [dr [-1 0 1]
          dc [-1 0 1]
          :when (not= [dr dc] [0 0])
          :let [nr (+ r dr)
                nc (+ c dc)]
          :when (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))]
      [nr nc])))

(defn step [grid]
  (let [rows (count grid)
        cols (count (first grid))
        incremented-grid (mapv (fn [row] (mapv inc row)) grid)
        flashed (atom #{})]
    (loop [g incremented-grid]
      (let [flash-coords (for [r (range rows)
                               c (range cols)
                               :when (and (> (get-in g [r c]) 9)
                                          (not (@flashed [r c])))]
                           [r c])]
        (if (empty? flash-coords)
          [g (count @flashed)]
          (let [g (reduce (fn [acc [r c]]
                             (swap! flashed conj [r c])
                             (reduce (fn [g2 [nr nc]]
                                       (update-in g2 [nr nc] inc))
                                     acc
                                     (neighbors acc r c)))
                           g
                           flash-coords)]
            (recur g)))))))

(defn reset-flashed [grid flashed-count]
   (mapv (fn [row] (mapv #(if (> % 9) 0 %) row)) grid)
  )

(defn solve-part1 [grid steps]
  (loop [g grid
         step-count 0
         total-flashes 0]
    (if (= step-count steps)
      total-flashes
      (let [[next-grid flashes] (step g)
            reset-grid (reset-flashed next-grid flashes)]
           
        (recur reset-grid (inc step-count) (+ total-flashes flashes))))))


(defn -main []
  (let [input (slurp "input.txt")
        initial-grid (parse-input input)]
    (println (solve-part1 initial-grid 100))))

(-main)
