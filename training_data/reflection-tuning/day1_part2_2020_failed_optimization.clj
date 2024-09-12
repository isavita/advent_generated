(ns advent-of-code.day1
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(Integer/parseInt %))))

(defn find-two-sum [numbers target]
  (let [number-set (set numbers)]
    (first (for [n numbers
                 :let [complement (- target n)]
                 :when (and (number-set complement)
                            (not= n complement))]
             (* n complement)))))

(defn find-three-sum [numbers target]
  (let [sorted-numbers (sort numbers)]
    (first (for [i (range (- (count sorted-numbers) 2))
                 :let [n (nth sorted-numbers i)
                       remaining (- target n)
                       left (inc i)
                       right (dec (count sorted-numbers))]
                 :while (< n remaining)]
             (loop [l left
                    r right]
               (if (< l r)
                 (let [sum (+ (nth sorted-numbers l) (nth sorted-numbers r))]
                   (cond
                     (= sum remaining) (* n (nth sorted-numbers l) (nth sorted-numbers r))
                     (< sum remaining) (recur (inc l) r)
                     :else (recur l (dec r))))
                 nil))))))

(defn solve-part1 [input]
  (find-two-sum (parse-input input) 2020))

(defn solve-part2 [input]
  (find-three-sum (parse-input input) 2020))

;; Example usage:
;; (solve-part1 input-string)
;; (solve-part2 input-string)
