(ns triangle-validator
  (:require [clojure.string :as str]))

(defn valid-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ c a) b)))

(defn parse-input [file]
  (->> (slurp file)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn part1 [numbers]
  (->> numbers
       (partition 3)
       (filter valid-triangle?)
       count))

(defn part2 [numbers]
  (->> numbers
       (partition 9)
       (mapcat (fn [group]
                 (apply map vector (partition 3 group))))
       (filter valid-triangle?)
       count))

(defn solve []
  (let [numbers (parse-input "input.txt")]
    (println "Part 1:" (part1 numbers))
    (println "Part 2:" (part2 numbers))))

(solve)
