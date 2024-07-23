
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x char]
                                     (when (= char \#) [x y 0]))
                                   line)))
       (apply concat)
       (filter some?)))

(defn neighbors [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not= [dx dy dz] [0 0 0])]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn simulate-cycle [active-cubes]
  (let [neighbor-counts (reduce (fn [counts cube]
                                   (reduce (fn [c n]
                                             (update c n (fnil inc 0)))
                                           counts (neighbors cube)))
                                 {}
                                 active-cubes)]
    (set (for [[coord count] neighbor-counts
               :when (or (= count 3) (and (= count 2) (active-cubes coord)))]
               coord))))

(defn main []
  (let [active-cubes (set (read-input "input.txt"))]
    (loop [cubes active-cubes cycles 6]
      (if (zero? cycles)
        (println (count cubes))
        (recur (simulate-cycle cubes) (dec cycles))))))

(main)
