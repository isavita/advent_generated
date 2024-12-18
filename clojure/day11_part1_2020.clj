
(ns seating-system
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (vec (map vec (str/split-lines input))))

(defn get-adjacent-seats [layout row col]
  (let [rows (count layout)
        cols (count (first layout))
        neighbors (for [dr [-1 0 1]
                        dc [-1 0 1]
                        :when (not (and (= dr 0) (= dc 0)))
                        :let [nr (+ row dr)
                              nc (+ col dc)]
                        :when (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))]
                    (get-in layout [nr nc]))]
    neighbors))

(defn count-occupied-adjacent [layout row col]
  (->> (get-adjacent-seats layout row col)
       (filter #(= % \#))
       count))

(defn apply-rules [layout]
  (let [rows (count layout)
        cols (count (first layout))]
    (vec (for [row (range rows)]
           (vec (for [col (range cols)]
                  (let [seat (get-in layout [row col])
                        occupied-count (count-occupied-adjacent layout row col)]
                    (cond
                      (and (= seat \L) (= occupied-count 0)) \#
                      (and (= seat \#) (>= occupied-count 4)) \L
                      :else seat))))))))

(defn stabilize [layout]
  (loop [current layout]
    (let [next-layout (apply-rules current)]
      (if (= current next-layout)
        current
        (recur next-layout)))))

(defn count-occupied [layout]
  (->> layout
       (map (fn [row] (filter #(= % \#) row)))
       (map count)
       (reduce +)))

(defn solve [input]
  (-> input
      parse-input
      stabilize
      count-occupied))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
