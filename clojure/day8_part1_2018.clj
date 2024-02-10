
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input [filename]
  (with-open [reader (io/reader filename)]
    (let [line (first (line-seq reader))
          parts (str/split line #"\s+")
          numbers (map #(Integer/parseInt %) parts)]
      numbers)))

(defn parse-tree [data index]
  (let [child-count (nth data index)
        meta-count (nth data (inc index))
        index (inc (inc index))]
    (loop [i 0
           sum 0
           idx index]
      (if (< i child-count)
        (let [[child-sum new-idx] (parse-tree data idx)]
          (recur (inc i) (+ sum child-sum) new-idx))
        (loop [i 0
               sum sum
               idx idx]
          (if (< i meta-count)
            (recur (inc i) (+ sum (nth data (+ idx i))) idx)
            [sum (+ idx meta-count)]))))))

(let [numbers (read-input "input.txt")
      [sum _] (parse-tree numbers 0)]
  (println sum))
