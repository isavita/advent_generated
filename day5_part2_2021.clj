(ns hydrothermal-venture.part-two
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map #(str/split % #",| -> ") (line-seq rdr)))))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) line))

(defn points-in-line [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
    (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1])
    :else     (let [dx (if (> x1 x2) -1 1)
                    dy (if (> y1 y2) -1 1)]
                (map vector
                     (take (inc (Math/abs (- x1 x2))) (iterate #(+ % dx) x1))
                     (take (inc (Math/abs (- y1 y2))) (iterate #(+ % dy) y1))))))

(defn count-overlaps [lines]
  (let [points (mapcat points-in-line lines)
        point-counts (frequencies points)]
    (count (filter #(>= % 2) (vals point-counts)))))

(defn solve [filename]
  (->> filename
       read-input
       (map parse-line)
       count-overlaps))

(println (solve "input.txt"))

