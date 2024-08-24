(ns lumberyard
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (vec (map vec (line-seq rdr)))))

(defn count-adjacent [grid x y char]
  (reduce + (for [dx [-1 0 1] dy [-1 0 1]
                  :when (not (and (= dx 0) (= dy 0)))
                  :let [nx (+ x dx) ny (+ y dy)]
                  :when (and (>= nx 0) (< nx (count grid))
                             (>= ny 0) (< ny (count (first grid)))
                             (= (get-in grid [nx ny]) char))]
              1)))

(defn next-state [grid x y]
  (let [current (get-in grid [x y])]
    (cond
      (= current \.) (if (>= (count-adjacent grid x y \|) 3) \| \.)
      (= current \|) (if (>= (count-adjacent grid x y \#) 3) \# \|)
      (= current \#) (if (and (>= (count-adjacent grid x y \#) 1)
                              (>= (count-adjacent grid x y \|) 1)) \# \.))))

(defn simulate-minute [grid]
  (vec (map-indexed (fn [x row]
                      (vec (map-indexed (fn [y _] (next-state grid x y)) row)))
                    grid)))

(defn resource-value [grid]
  (let [wooded (count (filter #(= \| %) (apply concat grid)))
        lumberyards (count (filter #(= \# %) (apply concat grid)))]
    (* wooded lumberyards)))

(defn main []
  (loop [grid (read-input) minutes 10]
    (if (zero? minutes)
      (println (resource-value grid))
      (recur (simulate-minute grid) (dec minutes)))))

(main)