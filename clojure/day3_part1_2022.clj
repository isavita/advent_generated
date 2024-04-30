(ns day3
  (:require [clojure.string :as str]))

(defn priority [c]
  (if (Character/isUpperCase c)
    (+ (- (int c) 64) 26)
    (+ (- (int c) 96) 0)))

(defn find-common [comp1 comp2]
  (first (filter #(str/includes? comp2 (str %)) comp1)))

(defn process-rucksack [line]
  (let [mid (quot (count line) 2)
        comp1 (subs line 0 mid)
        comp2 (subs line mid)]
    (priority (find-common comp1 comp2))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [rucksacks (line-seq rdr)]
      (println (reduce + (map process-rucksack rucksacks))))))

(-main)