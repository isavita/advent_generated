(ns day3-part2
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (vec (map vec (str/split-lines (slurp filename)))))

(defn adjacent-positions [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= [dx dy] [0 0])]
    [(+ x dx) (+ y dy)]))

(defn in-bounds? [schematic [x y]]
  (and (>= x 0) (< x (count schematic))
       (>= y 0) (< y (count (first schematic)))))

(defn digit? [c]
  (Character/isDigit c))

(defn extract-number [schematic [x y]]
  (loop [left y, right y, num ""]
    (if (and (>= left 0) (digit? (get-in schematic [x left])))
      (recur (dec left) right num)
      (if (and (< right (count (first schematic))) (digit? (get-in schematic [x right])))
        (recur left (inc right) num)
        (Integer/parseInt (str/join (subvec (schematic x) (inc left) right)))))))

(defn gear-ratio [schematic [x y]]
  (let [adjacent-nums (->> (adjacent-positions [x y])
                           (filter (partial in-bounds? schematic))
                           (filter #(digit? (get-in schematic %)))
                           (map #(extract-number schematic %))
                           (distinct))]
    (if (= 2 (count adjacent-nums))
      (apply * adjacent-nums)
      0)))

(defn solve [filename]
  (let [schematic (parse-input filename)]
    (->> (for [x (range (count schematic))
               y (range (count (first schematic)))
               :when (= \* (get-in schematic [x y]))]
           (gear-ratio schematic [x y]))
         (reduce +))))

(println (solve "input.txt"))
