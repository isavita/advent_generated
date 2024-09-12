(ns rock-paper-scissors
  (:require [clojure.string :as str]))

(def move-scores {"X" 1, "Y" 2, "Z" 3})
(def outcome-scores {"lose" 0, "draw" 3, "win" 6})

(def win-conditions {"A" "Y", "B" "Z", "C" "X"})
(def draw-conditions {"A" "X", "B" "Y", "C" "Z"})

(defn calculate-outcome [opponent ours]
  (cond
    (= (get win-conditions opponent) ours) "win"
    (= (get draw-conditions opponent) ours) "draw"
    :else "lose"))

(defn calculate-score-part1 [[opponent ours]]
  (+ (get move-scores ours)
     (get outcome-scores (calculate-outcome opponent ours))))

(def required-outcomes {"X" "lose", "Y" "draw", "Z" "win"})
(def move-to-choose
  {"A" {"lose" "Z", "draw" "X", "win" "Y"}
   "B" {"lose" "X", "draw" "Y", "win" "Z"}
   "C" {"lose" "Y", "draw" "Z", "win" "X"}})

(defn calculate-score-part2 [[opponent outcome]]
  (let [required-outcome (get required-outcomes outcome)
        our-move (get-in move-to-choose [opponent required-outcome])]
    (+ (get move-scores our-move)
       (get outcome-scores required-outcome))))

(defn solve-puzzle [calculate-score-fn]
  (->> (slurp "input.txt")
       str/split-lines
       (map #(str/split % #" "))
       (map calculate-score-fn)
       (reduce +)))

(println "Part 1:" (solve-puzzle calculate-score-part1))
(println "Part 2:" (solve-puzzle calculate-score-part2))
