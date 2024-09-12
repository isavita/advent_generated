(ns day2
  (:require [clojure.string :as str]))

(defn count-twos-and-threes [id]
  (let [freq (frequencies id)
        counts (set (vals freq))]
    [(if (counts 2) 1 0)
     (if (counts 3) 1 0)]))

(defn checksum [ids]
  (let [counts (apply map + (map count-twos-and-threes ids))]
    (apply * counts)))

(defn diff-by-one? [id1 id2]
  (= 1 (count (filter identity (map not= id1 id2)))))

(defn find-similar-ids [ids]
  (first
   (for [id1 ids
         id2 ids
         :when (and (not= id1 id2) (diff-by-one? id1 id2))]
     [id1 id2])))

(defn common-letters [id1 id2]
  (apply str (map #(if (= %1 %2) %1 "") id1 id2)))

(defn solve-part1 [input]
  (checksum (str/split-lines input)))

(defn solve-part2 [input]
  (let [ids (str/split-lines input)
        [id1 id2] (find-similar-ids ids)]
    (common-letters id1 id2)))

;; Example usage:
(def sample-input "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab")
(println "Part 1 (sample):" (solve-part1 sample-input))

(def puzzle-input (slurp "input.txt")) ; Assuming the input is in a file named input.txt
(println "Part 1:" (solve-part1 puzzle-input))
(println "Part 2:" (solve-part2 puzzle-input))
