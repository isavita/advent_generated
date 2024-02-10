
(ns day02.core
(:require [clojure.string :as str]
          [clojure.java.io :as io]))

(defn count-combinations
"counts the combinations of containers that can sum up to the target amount"
[containers target index]
(if (= target 0)
  1
  (if (or (< target 0) (>= index (count containers)))
    0
    (+ (count-combinations containers (- target (nth containers index)) (inc index))
       (count-combinations containers target (inc index))))))

(defn part-two []
(with-open [xin (io/reader "input.txt")]
  (let [containers (doall (map #(Integer/parseInt %) (line-seq xin)))]
    (count-combinations containers 150 0))))

(println (str (part-two)))
