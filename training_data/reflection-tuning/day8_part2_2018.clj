(ns day8
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (->> (slurp file)
       (str/trim)
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defn process-node [numbers]
  (let [[child-count metadata-count & rest] numbers
        [children rest metadata sum-metadata] 
        (loop [i 0
               rest rest
               children []
               sum 0]
          (if (< i child-count)
            (let [[child rest' child-sum] (process-node rest)]
              (recur (inc i) rest' (conj children child) (+ sum child-sum)))
            [children 
             (drop metadata-count rest) 
             (take metadata-count rest)
             (+ sum (reduce + (take metadata-count rest)))]))
        node-value (if (zero? child-count)
                     (reduce + metadata)
                     (reduce + (for [idx metadata
                                     :when (<= 1 idx (count children))]
                                 (get-in children [(dec idx) :value] 0))))]
    [{:children children
      :metadata metadata
      :value node-value}
     rest
     (+ sum-metadata node-value)]))

(defn solve [input]
  (let [[root _ sum-metadata root-value] (process-node input)]
    (println "Part 1:" sum-metadata)
    (println "Part 2:" (:value root))))

(def input (parse-input "input.txt"))
(solve input)
