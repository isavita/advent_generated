
(ns day21
  (:require [clojure.java.io :as io]))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        grid (vec (map vec lines))
        rows (count grid)
        cols (count (first grid))]
    {:grid grid
     :rows rows
     :cols cols
     :start (first (for [r (range rows)
                          c (range cols)
                          :when (= \S (get-in grid [r c]))]
                      [r c]))}))

(defn neighbors [[r c] rows cols]
  (filter (fn [[nr nc]]
            (and (>= nr 0) (< nr rows)
                 (>= nc 0) (< nc cols)))
          [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]]))

(defn solve [input steps]
  (let [{:keys [grid rows cols start]} (parse-input input)
        reachable (fn [start-set]
                    (->> start-set
                         (mapcat #(neighbors % rows cols))
                         (filter (fn [[r c]] (not= \# (get-in grid [r c]))))
                         set))]
     (count (nth (iterate reachable #{start}) steps))))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve input 64)]
    (println result)))

(-main)
