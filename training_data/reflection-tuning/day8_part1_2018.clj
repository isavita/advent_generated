(ns day8.core
  (:require [clojure.string :as str]))

(defn parse-node [input]
  (let [[child-count metadata-count & rest] input
        [remaining child-sum] (loop [remaining rest
                                     children-left child-count
                                     sum 0]
                                (if (zero? children-left)
                                  [remaining sum]
                                  (let [[new-remaining child-sum] (parse-node remaining)]
                                    (recur new-remaining
                                           (dec children-left)
                                           (+ sum child-sum)))))
        metadata (take metadata-count remaining)
        metadata-sum (reduce + metadata)]
    [(drop metadata-count remaining) (+ child-sum metadata-sum)]))

(defn solve-day8 []
  (let [input (->> (slurp "input.txt")
                   (str/trim)
                   (str/split-lines)
                   (first)
                   (re-seq #"\d+")
                   (mapv #(Integer/parseInt %)))
        [_ total-sum] (parse-node input)]
    (println total-sum)))

(solve-day8)
