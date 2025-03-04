
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
          :let [nr (+ r dr)
                nc (+ c dc)]
          :when (and (not= [dr dc] [0 0])
                     (<= 0 nr (dec rows))
                     (<= 0 nc (dec cols)))]
      [nr nc])))

(defn step [grid]
  (let [rows (count grid)
        cols (count (first grid))
        incremented (mapv (fn [row] (mapv inc row)) grid)
        flashed (atom #{})]
    (loop [g incremented]
      (let [flashing (for [r (range rows)
                           c (range cols)
                           :when (and (> (get-in g [r c]) 9)
                                      (not (@flashed [r c])))]
                       [r c])]
        (if (empty? flashing)
          g
          (let [g (reduce (fn [acc [r c]]
                             (reduce (fn [acc2 [nr nc]]
                                       (update-in acc2 [nr nc] inc))
                                     acc
                                     (neighbors acc r c)))
                           g
                           flashing)]
            (swap! flashed into flashing)
            (recur g)))))))

(defn reset-flashed [grid]
  (mapv (fn [row] (mapv #(if (> % 9) 0 %) row)) grid))


(defn solve-part1 [input]
   (loop [grid (parse-input input)
         i 0
         total-flashes 0]
     (if (= i 100)
      total-flashes
       (let [stepped-grid (step grid)
             reset-grid (reset-flashed stepped-grid)
             flashes-this-step (reduce + (map (fn[row] (count (filter zero? row))) reset-grid))]
         (recur reset-grid (inc i) (+ total-flashes flashes-this-step))))))

(defn solve-part2 [input]
    (loop [grid (parse-input input)
           i 1]
    (let [stepped-grid (step grid)
          reset-grid (reset-flashed stepped-grid)]
       (if (every? zero? (flatten reset-grid))
         i
         (recur reset-grid (inc i))))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))

(-main)
