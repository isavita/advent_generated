
(ns day13
  (:require [clojure.string :as str]))

(defn transpose [grid]
  (apply mapv vector grid))

(defn find-reflection [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [i 1]
      (if (>= i rows)
        nil
        (if (let [top (subvec grid 0 i)
                  bottom (subvec grid i)]
              (every? true? (map = (reverse top) bottom)))
          i
          (recur (inc i)))))))

(defn summarize [patterns]
  (reduce
   (fn [sum pattern]
     (if-let [vertical (find-reflection (transpose pattern))]
       (+ sum vertical)
       (if-let [horizontal (find-reflection pattern)]
         (+ sum (* 100 horizontal))
         sum)))
   0
   patterns))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map #(str/split-lines %))
       (map #(mapv vec %))))

(let [input (slurp "input.txt")
      patterns (parse-input input)]
  (println (summarize patterns)))
