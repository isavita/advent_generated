(ns day15
  (:require [clojure.string :as str]))

(defn hash-algorithm [s]
  (reduce (fn [current-value c]
            (mod (* (+ current-value (int c)) 17) 256))
          0 s))

(defn sum-of-hash-results [input]
  (->> (str/split input #",")
       (map hash-algorithm)
       (reduce +)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (str/trim (slurp rdr))]
      (println (sum-of-hash-results input)))))

(-main)