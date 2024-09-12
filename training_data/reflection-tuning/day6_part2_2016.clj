(ns day6.core
  (:require [clojure.string :as str]))

(defn transpose [m]
  (apply map vector m))

(defn most-common-char [s]
  (key (apply max-key val (frequencies s))))

(defn least-common-char [s]
  (key (apply min-key val (frequencies s))))

(defn process-message [selector-fn]
  (->> (slurp "input.txt")
       (str/split-lines)
       (transpose)
       (map selector-fn)
       (apply str)))

(defn part1 []
  (process-message most-common-char))

(defn part2 []
  (process-message least-common-char))

(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))

(-main)
