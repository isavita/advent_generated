
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main [& _]
  (let [grid   (->> "input.txt" slurp str/split-lines)
        rows   (count grid)
        cols   (count (first grid))
        dirs   [[-1 -1] [-1 0] [-1 1]
                [0 -1]        [0 1]
                [1 -1]  [1 0]  [1 1]]]
    (println
      (transduce
        (comp
          (filter #(= \@ (nth (nth grid (first %)) (second %))))
          (map (fn [[y x]]
                 (count (filter (fn [[dy dx]]
                                  (let [ny (+ y dy) nx (+ x dx)]
                                    (and (< -1 ny rows)
                                         (< -1 nx cols)
                                         (= \@ (nth (nth grid ny) nx)))))
                                dirs))))
          (filter #(< % 4))
          (take-while (constantly true)))
        (completing (fn [acc _] (inc acc)))
        0
        (for [y (range rows) x (range cols)] [y x])))))

(-main)
