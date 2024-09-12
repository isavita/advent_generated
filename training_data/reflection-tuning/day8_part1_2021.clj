(ns day8-solution
  (:require [clojure.string :as str]))

(defn count-unique-digits [line]
  (->> (str/split line #"\|")
       (second)
       (str/trim)
       (str/split-lines)
       (mapcat #(str/split % #" "))
       (filter #(contains? #{2 3 4 7} (count %)))
       (count)))

(defn solve-day8 [input-file]
  (->> (slurp input-file)
       (str/split-lines)
       (map count-unique-digits)
       (reduce +)))

(println (solve-day8 "input.txt"))
