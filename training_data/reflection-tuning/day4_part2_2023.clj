(ns day4-part2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-numbers [s]
  (set (map #(Integer/parseInt %) (re-seq #"\d+" s))))

(defn parse-card [line]
  (let [[_ winning have] (str/split line #"[:|]")]
    [(parse-numbers winning) (parse-numbers have)]))

(defn count-matches [[winning have]]
  (count (set/intersection winning have)))

(defn process-cards [cards]
  (let [card-count (count cards)
        copies (vec (repeat card-count 1))]
    (reduce
     (fn [copies card-index]
       (let [matches (count-matches (nth cards card-index))
             current-copies (nth copies card-index)]
         (reduce
          (fn [c i]
            (if (< i card-count)
              (update c i + current-copies)
              c))
          copies
          (range (inc card-index) (+ card-index matches 1)))))
     copies
     (range card-count))))

(defn solve [input]
  (let [cards (map parse-card (str/split-lines input))
        final-copies (process-cards cards)]
    (reduce + final-copies)))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve input)]
    (println result)))

(-main)
