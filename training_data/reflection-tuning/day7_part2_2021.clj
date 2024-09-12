(ns day07
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/trim)
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(def triangular-number
  (memoize (fn [n] (quot (* n (inc n)) 2))))

(defn total-fuel-cost [positions target-pos part2?]
  (reduce + (map #(let [distance (Math/abs (- % target-pos))]
                    (if part2?
                      (triangular-number distance)
                      distance))
                 positions)))

(defn find-min-fuel-cost [positions part2?]
  (let [min-pos (apply min positions)
        max-pos (apply max positions)]
    (apply min (map #(total-fuel-cost positions % part2?)
                    (range min-pos (inc max-pos))))))

(defn solve-part1 [positions]
  (find-min-fuel-cost positions false))

(defn solve-part2 [positions]
  (find-min-fuel-cost positions true))

(defn -main []
  (let [positions (parse-input "input.txt")]
    (println "Part 1:" (solve-part1 positions))
    (println "Part 2:" (solve-part2 positions))))

(-main)
