(ns day1.core
  (:require [clojure.string :as str]))

(defn solve []
  (->> (slurp "input.txt")
       (str/split-lines)
       (map read-string)
       (reduce +)))

(println (solve))
