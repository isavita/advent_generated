(ns lanternfish
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [numbers (map #(Integer/parseInt %) (str/split input #","))]
    (reduce (fn [acc n] (update acc n inc)) (vec (repeat 9 0)) numbers)))

(defn simulate-day [fish]
  (-> (subvec fish 1)
      (conj (first fish))
      (update 6 + (first fish))))

(defn simulate-days [fish days]
  (nth (iterate simulate-day fish) days))

(defn solve [input days]
  (let [initial-state (parse-input input)
        final-state (simulate-days initial-state days)]
    (reduce + final-state)))

(defn -main []
  (let [input (str/trim (slurp "input.txt"))]
    (println "Part 1:" (solve input 80))
    (println "Part 2:" (solve input 256))))

(-main)
