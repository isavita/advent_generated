(ns day4.core
  (:require [clojure.string :as str]))

(defn parse-range [s]
  (mapv #(Integer/parseInt %) (str/split s #"-")))

(defn parse-line [line]
  (mapv parse-range (str/split line #",")))

(defn fully-contains? [[a b] [c d]]
  (or (<= a c d b)
      (<= c a b d)))

(defn overlaps? [[a b] [c d]]
  (and (<= a d) (<= c b)))

(defn count-pairs [pred pairs]
  (count (filter #(apply pred %) pairs)))

(let [pairs (map parse-line (str/split-lines (slurp "input.txt")))
      contained-count (count-pairs fully-contains? pairs)
      overlap-count (count-pairs overlaps? pairs)]
  (println "Part One:" contained-count)
  (println "Part Two:" overlap-count))
