
(ns day9
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map #(Integer/parseInt %) (str/split line #" ")))

(defn differences [nums]
  (map - (rest nums) nums))

(defn all-zeros? [nums]
  (every? zero? nums))

(defn extrapolate-next [nums]
  (loop [seqs [(vec nums)]]
    (let [current-seq (last seqs)
          diffs (differences current-seq)]
      (if (all-zeros? diffs)
        (reduce + 0 (map last seqs))
        (recur (conj seqs (vec diffs)))))))

(defn extrapolate-previous [nums]
  (loop [seqs [(vec nums)]]
    (let [current-seq (last seqs)
          diffs (differences current-seq)]
      (if (all-zeros? diffs)
        (reduce (fn [acc s] (- (first s) acc)) 0 (reverse seqs))
        (recur (conj seqs (vec diffs)))))))

(defn solve-part1 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map extrapolate-next)
       (reduce + 0)))

(defn solve-part2 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map extrapolate-previous)
       (reduce + 0)))

(defn main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))

(main)
