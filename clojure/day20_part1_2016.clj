(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-range [s]
  (let [[start end] (map #(Long/parseLong %) (str/split s #"-"))]
    [start end]))

(defn merge-ranges [ranges]
  (let [sorted (sort-by first ranges)]
    (reduce (fn [merged [start end]]
              (if (empty? merged)
                [[start end]]
                (let [[last-start last-end] (last merged)]
                  (if (>= start (inc last-end))
                    (conj merged [start end])
                    (conj (vec (butlast merged)) [last-start (max last-end end)])))))
            []
            sorted)))

(defn find-lowest-unblocked [ranges]
  (let [merged (merge-ranges ranges)]
    (loop [current 0
           [[start end] & rest] merged]
      (if (nil? start)
        current
        (if (< current start)
          current
          (recur (inc end) rest))))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [ranges (map parse-range (line-seq rdr))]
      (println (find-lowest-unblocked ranges)))))

(-main)