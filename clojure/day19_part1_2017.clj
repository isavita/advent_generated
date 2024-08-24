(ns day19
  (:require [clojure.java.io :as io]))

(defn find-start [grid]
  (first (for [x (range (count (first grid)))
               :when (= (get-in grid [0 x]) \|)]
           [0 x])))

(defn move [pos dir]
  (let [[y x] pos
        [dy dx] dir]
    [(+ y dy) (+ x dx)]))

(defn valid? [grid pos]
  (let [[y x] pos]
    (and (>= y 0) (< y (count grid))
         (>= x 0) (< x (count (first grid)))
         (not= (get-in grid pos) \space))))

(defn follow-path [grid]
  (let [start (find-start grid)
        directions [[1 0] [0 1] [-1 0] [0 -1]]]
    (loop [pos start
           dir [1 0]
           seen ""]
      (let [current (get-in grid pos)]
        (cond
          (= current \|) (recur (move pos dir) dir seen)
          (= current \-) (recur (move pos dir) dir seen)
          (Character/isLetter current) (recur (move pos dir) dir (str seen current))
          (= current \+) (let [new-dir (first (filter #(and (not= % dir)
                                                            (not= % (map - dir))
                                                            (valid? grid (move pos %)))
                                                      directions))]
                           (recur (move pos new-dir) new-dir seen))
          :else seen)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [grid (vec (map vec (line-seq rdr)))]
      (println (follow-path grid)))))

(-main)