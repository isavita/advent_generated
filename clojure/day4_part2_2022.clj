(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-range [s]
  (let [split (str/split s #"-")]
    [(Integer/parseInt (first split)) (Integer/parseInt (second split))]))

(defn count-overlapping-ranges []
  (with-open [reader (io/reader "input.txt")]
    (let [lines (line-seq reader)
          pairs (map #(str/split % #",") lines)
          count (reduce
                  (fn [acc pair]
                    (let [left (parse-range (first pair))
                          right (parse-range (second pair))]
                      (if (and (<= (first left) (second right))
                               (>= (second left) (first right)))
                        (inc acc)
                        acc)))
                  0
                  pairs)]
      (println count))))

(count-overlapping-ranges)