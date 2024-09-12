(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(def directions [:north :east :south :west])

(defn turn [current-dir turn-dir]
  (let [current-index (.indexOf directions current-dir)
        turn-amount (if (= turn-dir \L) -1 1)]
    (nth directions (mod (+ current-index turn-amount) 4))))

(defn move [[x y] direction distance]
  (case direction
    :north [x (+ y distance)]
    :east [(+ x distance) y]
    :south [x (- y distance)]
    :west [(- x distance) y]))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn parse-instruction [instruction]
  (let [turn-dir (first instruction)
        distance (Integer/parseInt (subs instruction 1))]
    [turn-dir distance]))

(defn solve-part1 [input]
  (loop [instructions (map parse-instruction (str/split input #", "))
         position [0 0]
         direction :north]
    (if (empty? instructions)
      (manhattan-distance position)
      (let [[turn-dir distance] (first instructions)
            new-direction (turn direction turn-dir)
            new-position (move position new-direction distance)]
        (recur (rest instructions) new-position new-direction)))))

(defn solve-part2 [input]
  (loop [instructions (map parse-instruction (str/split input #", "))
         position [0 0]
         direction :north
         visited #{[0 0]}]
    (if (empty? instructions)
      nil  ; No repeated position found
      (let [[turn-dir distance] (first instructions)
            new-direction (turn direction turn-dir)]
        (loop [steps distance
               current-pos position
               current-visited visited]
          (if (zero? steps)
            (recur (rest instructions) current-pos new-direction current-visited)
            (let [next-pos (move current-pos new-direction 1)]
              (if (contains? current-visited next-pos)
                (manhattan-distance next-pos)
                (recur (dec steps) next-pos (conj current-visited next-pos))))))))))

(defn solve [input]
  {:part1 (solve-part1 input)
   :part2 (solve-part2 input)})

;; Example usage:
;; (solve "R5, L5, R5, R3")
