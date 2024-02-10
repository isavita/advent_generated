(ns hydrothermal-venture.core
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map #(str/split % #",| -> ") (line-seq rdr)))))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) line))

(defn horizontal-or-vertical? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn points-in-line [[x1 y1 x2 y2]]
  (let [x-range (range (min x1 x2) (inc (max x1 x2)))
        y-range (range (min y1 y2) (inc (max y1 y2)))]
    (if (= x1 x2)
      (for [y y-range] [x1 y])
      (for [x x-range] [x y1]))))

(defn count-overlaps [lines]
  (let [points (mapcat points-in-line (filter horizontal-or-vertical? lines))
        point-counts (frequencies points)]
    (count (filter #(>= % 2) (vals point-counts)))))

(defn solve [filename]
  (->> filename
       read-input
       (map parse-line)
       count-overlaps))

(println (solve "input.txt"))

