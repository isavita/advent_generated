(ns calorie-counting
  (:require [clojure.string :as str]))

(defn process-calories [input]
  (reduce (fn [[max-calories current-sum] line]
            (if (str/blank? line)
              [max-calories 0]
              (let [calories (Integer/parseInt line)
                    new-sum (+ current-sum calories)]
                [(max max-calories new-sum) new-sum])))
          [0 0]
          input))

(defn solve-calorie-counting []
  (->> (slurp "input.txt")
       str/split-lines
       process-calories
       first
       println))

(solve-calorie-counting)
