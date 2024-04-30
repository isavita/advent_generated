(ns day6.fire-hazard-part2
  (:require [clojure.string :as str]))

; Function to parse a line and extract the command and coordinates
(defn parse-line [line]
  (let [parts (re-seq #"\d+" line)
        coords (mapv #(Integer/parseInt %) parts)]
    [(if (.startsWith line "turn on") :on
       (if (.startsWith line "toggle") :toggle :off))
     (subvec coords 0 2) (subvec coords 2 4)]))

; Function to apply a command to the grid
(defn apply-command [grid [cmd start end]]
  (let [[x1 y1] start [x2 y2] end]
    (reduce (fn [g x]
              (reduce (fn [g' y]
                        (assoc-in g' [x y] (case cmd
                                             :on (inc (get-in g [x y] 0))
                                             :off (max 0 (dec (get-in g [x y] 0)))
                                             :toggle (+ 2 (get-in g [x y] 0)))))
                      g (range y1 (inc y2))))
            grid (range x1 (inc x2)))))

; Function to calculate total brightness
(defn total-brightness [grid]
  (reduce (fn [sum row] (+ sum (reduce + row))) 0 grid))

; Main function to read the file, process commands and print the result
(defn -main []
  (let [instructions (map parse-line (line-seq (clojure.java.io/reader "input.txt")))
        initial-grid (vec (repeat 1000 (vec (repeat 1000 0))))]
    (->> instructions
         (reduce apply-command initial-grid)
         total-brightness
         println)))

; Call the main function
(-main)
