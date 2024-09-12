(ns lanternfish
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(defn update-fish [timer]
  (if (zero? timer)
    [6 8]  ; Reset timer and create new fish
    [(dec timer)]))  ; Decrement timer

(defn simulate-day [fish]
  (vec (mapcat update-fish fish)))

(defn simulate-days [fish days]
  (nth (iterate simulate-day fish) days))

(defn solve [input days]
  (count (simulate-days (parse-input input) days)))

(defn -main [& args]
  (let [input (slurp (first args))
        result (solve input 80)]
    (println result)))
