(ns day1
  (:require [clojure.string :as str]))

(defn find-two-entries
  [numbers target]
  (let [set-numbers (set numbers)]
    (some #(let [other (- target %)]
             (when (contains? set-numbers other)
               (* % other)))
          numbers)))

(defn find-three-entries
  [numbers target]
  (some (fn [[x xs]]
          (if-let [result (find-two-entries xs (- target x))]
            (* x result)))
        (map (fn [x] [x (vec (remove #{x} numbers))])
             numbers)))

(defn -main []
  (let [input (slurp "input.txt")
        numbers (->> input
                    (str/split-lines)
                    (map #(Integer/parseInt %)))]
    (println (find-two-entries numbers 2020))
    (println (find-three-entries numbers 2020))))

(-main)