(ns solution
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn compare-values [left right]
  (cond
    (and (int? left) (int? right))
    (if (< left right) -1 (if (> left right) 1 0))

    (and (vector? left) (vector? right))
    (loop [l left r right]
      (cond
        (empty? l) (if (empty? r) 0 -1)
        (empty? r) 1
        :else (let [result (compare-values (first l) (first r))]
                (if (zero? result)
                  (recur (rest l) (rest r))
                  result))))

    (int? left)
    (compare-values [left] right)

    :else
    (compare-values left [right])))

(defn read-input []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (doall (map edn/read-string (remove str/blank? (line-seq rdr))))))

(defn part-one []
  (let [lines (read-input)]
    (loop [i 0 sum 0]
      (if (>= i (count lines))
        sum
        (let [left (nth lines i)
              right (nth lines (inc i))]
          (if (neg? (compare-values left right))
            (recur (+ i 2) (+ sum (/ i 2 1)))
            (recur (+ i 2) sum)))))))

(defn part-two []
  (let [lines (read-input)
        lines (conj lines [[2]] [[6]])]
    (let [sorted-lines (sort compare-values lines)]
      (* (+ 1 (.indexOf sorted-lines [[2]]))
         (+ 1 (.indexOf sorted-lines [[6]]))))))

(println (part-two))