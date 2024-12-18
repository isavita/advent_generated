
(ns day14
  (:require [clojure.string :as str]))

(defn parse-grid [input]
  (mapv vec (str/split-lines input)))

(defn tilt-north [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [col 0
           new-grid (vec (repeat rows (vec (repeat cols \.))))]
      (if (= col cols)
        new-grid
        (let [next-available (atom 0)]
          (recur (inc col)
                 (reduce
                  (fn [acc row]
                    (let [cell (get-in grid [row col])]
                      (cond
                        (= cell \O) (do
                                      (swap! next-available inc)
                                      (assoc-in acc [(- @next-available 1) col] \O))
                        (= cell \#) (do
                                      (reset! next-available (inc row))
                                      (assoc-in acc [row col] \#))
                        :else acc)))
                  new-grid
                  (range rows))))))))

(defn calculate-load [grid]
  (let [rows (count grid)]
    (reduce-kv
     (fn [acc row line]
       (+ acc (reduce (fn [sum cell]
                        (if (= cell \O)
                          (+ sum (- rows row))
                          sum))
                      0 line)))
     0 grid)))

(let [grid (parse-grid (slurp "input.txt"))
      tilted-grid (tilt-north grid)]
  (println (calculate-load tilted-grid)))
