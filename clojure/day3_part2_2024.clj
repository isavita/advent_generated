
(ns day3
  (:require [clojure.string :as str]))

(defn parse-mul [s]
  (let [matches (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" s)]
    (map (fn [[_ a b]] [(Integer/parseInt a) (Integer/parseInt b)]) matches)))

(defn solve-part1 [input]
  (let [muls (parse-mul input)]
    (reduce + (map (fn [[a b]] (* a b)) muls))))

(defn solve-part2 [input]
  (loop [input input
         enabled? true
         total 0]
    (if (empty? input)
      total
      (let [do-idx (str/index-of input "do()")
            dont-idx (str/index-of input "don't()")
            mul-match (re-find #"mul\((\d{1,3}),(\d{1,3})\)" input)
            next-idx (if mul-match (inc (.indexOf input (first mul-match))) Integer/MAX_VALUE)]
        
        (cond
          (and do-idx (< do-idx (min next-idx (or dont-idx Integer/MAX_VALUE))))
          (recur (subs input (+ do-idx 4)) true total)
          
          (and dont-idx (< dont-idx (min next-idx (or do-idx Integer/MAX_VALUE))))
          (recur (subs input (+ dont-idx 6)) false total)

          (and mul-match (< (.indexOf input (first mul-match)) (min (or do-idx Integer/MAX_VALUE) (or dont-idx Integer/MAX_VALUE))))
            (let [[_ a b] mul-match
                  a (Integer/parseInt a)
                  b (Integer/parseInt b)
                  new-total (if enabled? (+ total (* a b)) total)]
              (recur (subs input next-idx) enabled? new-total))

          :else (recur (subs input 1) enabled? total)
          )))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))

(-main)
