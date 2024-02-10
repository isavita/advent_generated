
(ns solution
  (:require [clojure.string :as str]))

(defn read-lines [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (doall (line-seq reader))))

(def lines (read-lines "input.txt"))

(defn parse-int [s]
  (Integer/parseInt s))

(defn solve []
  (let [caloriesList (atom [])
        currentCalories (atom 0)]
    (doseq [line lines]
      (if (str/blank? line)
        (do
          (swap! caloriesList conj @currentCalories)
          (reset! currentCalories 0))
        (do
          (swap! currentCalories + (parse-int line)))))
    (swap! caloriesList conj @currentCalories)
    (reset! caloriesList (sort > @caloriesList))
    (let [topThreeSum (reduce + (take 3 @caloriesList))]
      (println topThreeSum))))

(solve)
