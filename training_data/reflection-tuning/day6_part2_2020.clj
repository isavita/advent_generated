(ns day6.core
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)))

(defn count-any-yes [group]
  (->> group
       (apply str)
       set
       count))

(defn count-all-yes [group]
  (->> group
       (map set)
       (apply intersection)
       count))

(defn solve-puzzle [input]
  (let [groups (parse-input input)]
    {:part1 (reduce + (map count-any-yes groups))
     :part2 (reduce + (map count-all-yes groups))}))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve-puzzle input)]
    (println "Part 1:" (:part1 result))
    (println "Part 2:" (:part2 result))))

(-main)
