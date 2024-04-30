(ns day6.fire-hazard
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
                                             :on true
                                             :off false
                                             :toggle (not (get-in g [x y])))))
                      g (range y1 (inc y2))))
            grid (range x1 (inc x2)))))

; Function to count lights that are on
(defn count-lights-on [grid]
  (reduce (fn [sum row] (+ sum (count (filter identity row)))) 0 grid))

; Main function to read the file, process commands and print the result
(defn -main []
  (let [instructions (map parse-line (line-seq (clojure.java.io/reader "input.txt")))
        initial-grid (vec (repeat 1000 (vec (repeat 1000 false))))]
    (->> instructions
         (reduce apply-command initial-grid)
         count-lights-on
         println)))

; Call the main function
(-main)
