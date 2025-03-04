
(ns day13
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[coords folds] (str/split input #"\n\n")
        coords (map #(mapv parse-long (str/split % #",")) (str/split-lines coords))
        folds (map (fn [line]
                     (let [[axis val] (str/split (subs line 11) #"=")]
                       [(keyword axis) (parse-long val)]))
                   (str/split-lines folds))]
    [coords folds]))

(defn fold-point [point [axis fold-val]]
  (let [[x y] point]
    (case axis
      :x (if (> x fold-val) [(- (* 2 fold-val) x) y] point)
      :y (if (> y fold-val) [x (- (* 2 fold-val) y)] point))))

(defn fold-paper [coords fold]
  (->> coords
       (map #(fold-point % fold))
       set))

(defn solve-part1 [input]
  (let [[coords folds] (parse-input input)]
    (count (fold-paper coords (first folds)))))

(defn solve-part2 [input]
  (let [[coords folds] (parse-input input)
        folded-coords (reduce fold-paper coords folds)
        max-x (apply max (map first folded-coords))
        max-y (apply max (map second folded-coords))]
    (for [y (range (inc max-y))]
      (->> (for [x (range (inc max-x))]
             (if (folded-coords [x y]) \# \space))
           (apply str)))))


(defn -main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:")
     (run! println (solve-part2 input))
     )
  )

(-main)
