(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(def number-words
  {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5",
   "six" "6", "seven" "7", "eight" "8", "nine" "9"})

(defn find-digits [s]
  (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" s))

(defn word-to-digit [s]
  (get number-words s s))

(defn calibration-value [line]
  (let [digits (find-digits line)
        first-digit (word-to-digit (second (first digits)))
        last-digit (word-to-digit (second (last digits)))]
    (Integer/parseInt (str first-digit last-digit))))

(defn solve [input]
  (->> (str/split-lines input)
       (map calibration-value)
       (reduce +)))

;; Example usage:
;; (def input (slurp "path/to/input.txt"))
;; (println (solve input))
