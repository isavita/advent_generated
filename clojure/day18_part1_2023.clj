
(ns day18
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[dir len color] (str/split line #" ")]
    {:dir (case dir "R" :right "D" :down "L" :left "U" :up)
     :len (parse-long len)
     :color (subs color 2 8)}))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn calculate-area [instructions]
  (loop [instructions instructions
         x 0
         y 0
         perimeter 0
         area 0]
    (if (empty? instructions)
      (+ (/ area 2) (/ perimeter 2) 1)
      (let [{:keys [dir len]} (first instructions)
            [dx dy] (case dir :right [len 0] :down [0 len] :left [(- len) 0] :up [0 (- len)])
            new-x (+ x dx)
            new-y (+ y dy)]
        (recur (rest instructions)
               new-x
               new-y
               (+ perimeter len)
               (+ area (* x new-y) (- (* y new-x))))))))

(defn solve [input]
  (let [instructions (parse-input input)]
    (calculate-area instructions)))

(let [input (slurp "input.txt")]
  (println (solve input)))
