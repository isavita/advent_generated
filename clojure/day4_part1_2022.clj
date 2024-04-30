(ns day4
  (:require [clojure.string :as str]))

(defn parse-range [s]
  (let [[_ start end] (re-matches #"(\d+)-(\d+)" s)]
    [(Integer/parseInt start) (Integer/parseInt end)]))

(defn fully-contains? [[start1 end1] [start2 end2]]
  (and (<= start1 start2) (>= end1 end2)))

(defn count-fully-contained [pairs]
  (count (filter (fn [[range1 range2]]
                    (or (fully-contains? range1 range2)
                        (fully-contains? range2 range1)))
                  pairs)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [pairs (map #(str/split % #",")
                      (line-seq rdr))]
      (println (count-fully-contained (map (fn [[range1 range2]]
                                           [(parse-range range1) (parse-range range2)])
                                        pairs))))))

(-main)