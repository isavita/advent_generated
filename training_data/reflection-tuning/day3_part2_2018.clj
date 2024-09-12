(ns day3
  (:require [clojure.string :as str]))

(defn parse-claim [claim]
  (let [[id & rest] (re-seq #"\d+" claim)]
    (zipmap [:id :left :top :width :height] (map #(Integer/parseInt %) (cons id rest)))))

(defn mark-fabric [fabric {:keys [left top width height]}]
  (reduce (fn [fab y]
            (reduce (fn [f x]
                      (update-in f [y x] inc))
                    fab
                    (range left (+ left width))))
          fabric
          (range top (+ top height))))

(defn solve-puzzle []
  (let [claims (map parse-claim (str/split-lines (slurp "input.txt")))
        fabric (reduce mark-fabric (vec (repeat 1000 (vec (repeat 1000 0)))) claims)
        overlapping-inches (count (filter #(> % 1) (flatten fabric)))
        non-overlapping-claim (first (filter (fn [{:keys [left top width height]}]
                                               (every? #(= % 1)
                                                       (for [y (range top (+ top height))
                                                             x (range left (+ left width))]
                                                         (get-in fabric [y x]))))
                                             claims))]
    (println "Part 1:" overlapping-inches)
    (println "Part 2:" (:id non-overlapping-claim))))

(solve-puzzle)
