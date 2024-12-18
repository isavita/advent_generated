
(ns aoc.day18
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[dir len color] (str/split line #" ")
        color (subs color 2 (- (count color) 1))]
    {:dir (keyword (str dir))
     :len (parse-long len)
     :color color}))

(defn parse-line-part2 [line]
  (let [[_ _ color] (str/split line #" ")
        color (subs color 2 (- (count color) 1))
        len (Long/parseLong (subs color 0 5) 16)
        dir (case (subs color 5 6)
              "0" :R
              "1" :D
              "2" :L
              "3" :U)]
    {:dir dir
     :len len}))

(defn parse-input [input parse-fn]
  (map parse-fn (str/split-lines input)))

(defn calculate-area [instructions]
  (loop [instructions instructions
         x 0
         y 0
         perimeter 0
         area 0]
    (if (empty? instructions)
      (+ (/ area 2) (/ perimeter 2) 1)
      (let [{:keys [dir len]} (first instructions)
            [dx dy] (case dir
                      :R [len 0]
                      :D [0 len]
                      :L [(- len) 0]
                      :U [0 (- len)])
            new-x (+ x dx)
            new-y (+ y dy)]
        (recur (rest instructions)
               new-x
               new-y
               (+ perimeter len)
               (+ area (* x new-y) (- (* y new-x))))))))

(let [input (slurp "input.txt")
      instructions (parse-input input parse-line)
      instructions-part2 (parse-input input parse-line-part2)]
  (println "Part 1:" (calculate-area instructions))
  (println "Part 2:" (calculate-area instructions-part2)))
