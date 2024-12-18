
(ns lanternfish
  (:require [clojure.string :as str]))

(defn parse-input [input-str]
  (->> (str/split input-str #",")
       (map #(Integer/parseInt %))))

(defn simulate-day [fish]
  (let [new-fish (filter #(= 0 %) fish)
        updated-fish (map #(if (= 0 %) 6 (dec %)) fish)]
    (concat updated-fish (repeat (count new-fish) 8))))

(defn simulate [initial-fish days]
  (loop [fish initial-fish
         day 0]
    (if (= day days)
      (count fish)
      (recur (simulate-day fish) (inc day)))))

(defn solve []
  (let [input (slurp "input.txt")
        initial-fish (parse-input input)
        result (simulate initial-fish 80)]
    (println result)))

(solve)
