(ns scratchcards
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-card [line]
  (let [[_ numbers] (str/split line #": ")
        [winning yours] (str/split numbers #" \| ")
        parse-nums #(set (map read-string (re-seq #"\d+" %)))]
    [(parse-nums winning) (parse-nums yours)]))

(defn calculate-points [winning yours]
  (let [matches (count (set/intersection winning yours))]
    (if (pos? matches)
      (Math/pow 2 (dec matches))
      0)))

(defn process-cards [input]
  (->> input
       str/split-lines
       (map parse-card)
       (map #(apply calculate-points %))
       (map int)
       (reduce +)))

(defn solve [input]
  (process-cards input))

;; Example usage:
(def sample-input
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(println (solve sample-input))
