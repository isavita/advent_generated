
(ns day18
  (:require [clojure.string :as str]))

(defn evaluate [expr]
  (loop [s expr]
    (if-let [start (str/last-index-of s "(")]
      (let [end (str/index-of s ")" start)
            sub (subs s (inc start) end)
            sub-val (evaluate sub)]
        (recur (str (subs s 0 start) sub-val (subs s (inc end)))))
      (let [tokens (str/split s #"\s+")
            [result & rest-tokens]
              (reduce
                (fn [[acc op] tok]
                  (cond
                    (#{"+" "*"} tok) [acc tok]
                    (= op "+") [(+ acc (Long/parseLong tok)) nil]
                    (= op "*") [(* acc (Long/parseLong tok)) nil]
                    :else [(Long/parseLong tok) nil]))
                [nil nil]
                tokens)]
        result))))

(defn -main [& _]
  (->> (slurp "input.txt")
       str/split-lines
       (remove str/blank?)
       (map evaluate)
       (reduce +)
       println))

(-main)
