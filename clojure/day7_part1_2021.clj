(ns day7
  (:require [clojure.string :as str]))

(defn fuel-cost [positions target]
  (apply + (map #(Math/abs (- % target)) positions)))

(defn -main []
  (let [positions (map #(Integer/parseInt %) (str/split (slurp "input.txt") #","))]
    (let [min-fuel (apply min (map #(fuel-cost positions %) (range (apply min positions) (inc (apply max positions)))))]
      (println "Minimum fuel cost:" min-fuel))))

(-main)