(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn calculate-fuel [mass]
  (-> mass
      (/ 3)
      int
      (- 2)
      (max 0)))

(defn total-fuel [mass]
  (->> mass
       (iterate calculate-fuel)
       (take-while pos?)
       rest
       (reduce +)))

(defn solve-part1 [masses]
  (->> masses
       (map calculate-fuel)
       (reduce +)))

(defn solve-part2 [masses]
  (->> masses
       (map total-fuel)
       (reduce +)))

(defn -main []
  (let [masses (->> (slurp "input.txt")
                    str/split-lines
                    (map #(Integer/parseInt %)))]
    (println "Part 1:" (solve-part1 masses))
    (println "Part 2:" (solve-part2 masses))))

(-main)
