(ns solution
  (:require [clojure.java.io :as io]))

(defn read-grid []
  (with-open [r (io/reader "input.txt")]
    (vec (for [line (line-seq r)]
           (vec (map #(if (= % \#) 1 0) line))))))

(defn count-bugs [grid x y]
  (let [directions [[0 -1] [0 1] [-1 0] [1 0]]]
    (reduce + (for [[dx dy] directions
                    :let [nx (+ x dx) ny (+ y dy)]
                    :when (and (>= nx 0) (< nx 5) (>= ny 0) (< ny 5))]
                (get-in grid [ny nx] 0)))))

(defn next-grid [grid]
  (vec (for [y (range 5)]
         (vec (for [x (range 5)]
                (let [bugs (count-bugs grid x y)]
                  (cond
                    (and (= 1 (get-in grid [y x])) (not= bugs 1)) 0
                    (and (= 0 (get-in grid [y x])) (<= bugs 2) (>= bugs 1)) 1
                    :else (get-in grid [y x]))))))))

(defn grid-to-string [grid]
  (apply str (for [row grid] (apply str row))))

(defn biodiversity-rating [grid]
  (reduce + (for [y (range 5) x (range 5)
                  :when (= 1 (get-in grid [y x]))]
              (bit-shift-left 1 (+ (* y 5) x)))))

(defn -main []
  (let [grid (read-grid)
        seen (atom #{})]
    (loop [current-grid grid]
      (let [grid-str (grid-to-string current-grid)]
        (if (@seen grid-str)
          (println (biodiversity-rating current-grid))
          (do
            (swap! seen conj grid-str)
            (recur (next-grid current-grid))))))))

(-main)