(ns day5
  (:require [clojure.string :as str]))

(defn has-three-vowels? [s]
  (>= (count (re-seq #"[aeiou]" s)) 3))

(defn has-double-letter? [s]
  (boolean (some #(= (first %) (second %)) (partition 2 1 s))))

(defn no-forbidden-strings? [s]
  (not (re-find #"ab|cd|pq|xy" s)))

(defn nice-string? [s]
  (and (has-three-vowels? s)
       (has-double-letter? s)
       (no-forbidden-strings? s)))

(defn count-nice-strings [lines]
  (count (filter nice-string? lines)))

(defn solve-day5 []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (count-nice-strings (line-seq rdr))))

(println (solve-day5))
