(ns binary-diagnostic
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       str/split-lines
       (mapv #(mapv (comp parse-long str) %))))

(defn most-common-bit [nums pos]
  (let [sum (reduce #(+ %1 (nth %2 pos)) 0 nums)
        half (/ (count nums) 2)]
    (if (>= sum half) 1 0)))

(defn binary-to-decimal [bits]
  (Integer/parseInt (apply str bits) 2))

(defn calculate-rates [nums]
  (let [bit-count (count (first nums))
        gamma-bits (mapv #(most-common-bit nums %) (range bit-count))
        epsilon-bits (mapv #(- 1 %) gamma-bits)]
    [(binary-to-decimal gamma-bits)
     (binary-to-decimal epsilon-bits)]))

(defn filter-by-bit-criteria [nums pos most-common?]
  (let [target-bit (if most-common?
                     (most-common-bit nums pos)
                     (- 1 (most-common-bit nums pos)))]
    (filter #(= (nth % pos) target-bit) nums)))

(defn find-rating [nums most-common?]
  (loop [remaining nums
         pos 0]
    (if (= 1 (count remaining))
      (binary-to-decimal (first remaining))
      (recur (filter-by-bit-criteria remaining pos most-common?) (inc pos)))))

(defn solve-part1 [nums]
  (apply * (calculate-rates nums)))

(defn solve-part2 [nums]
  (* (find-rating nums true)
     (find-rating nums false)))

(defn -main []
  (let [nums (parse-input "input.txt")]
    (println "Part 1:" (solve-part1 nums))
    (println "Part 2:" (solve-part2 nums))))

(-main)
