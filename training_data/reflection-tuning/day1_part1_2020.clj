(ns advent-of-code.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn find-2020-pair [numbers]
  (let [number-set (set numbers)]
    (first (for [n numbers
                 :let [complement (- 2020 n)]
                 :when (and (not= n complement)
                            (contains? number-set complement))]
             (* n complement)))))

(defn solve-day1 []
  (with-open [rdr (io/reader "input.txt")]
    (let [numbers (->> (line-seq rdr)
                       (map #(Integer/parseInt %)))]
      (find-2020-pair numbers))))

(println (solve-day1))
