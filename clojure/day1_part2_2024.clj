(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (let [lines (slurp filename)]
    (reduce
      (fn [[left-list right-list] line]
        (let [[left right] (map #(Integer/parseInt %) (str/split line #"\s+"))]
          [(conj left-list left) (conj right-list right)]))
      [[] []]
      (str/split-lines lines))))

(defn count-frequencies [coll]
  (reduce (fn [freq-map num]
            (assoc freq-map num (inc (get freq-map num 0))))
          {}
          coll))

(defn similarity-score [left-list right-list]
  (let [right-freq (count-frequencies right-list)]
    (reduce + (map (fn [num] (* num (get right-freq num 0))) left-list))))

(defn -main []
  (let [[left-list right-list] (parse-input "input.txt")
        score (similarity-score left-list right-list)]
    (println "Similarity score:" score)))

(-main)
