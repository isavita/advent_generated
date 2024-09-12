(ns hydrothermal-venture
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(mapv read-string (str/split % #","))
        (str/split line #" -> ")))

(defn range-inclusive [a b]
  (if (<= a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn points-on-line [[[x1 y1] [x2 y2]]]
  (if (or (= x1 x2) (= y1 y2))
    (for [x (range-inclusive x1 x2)
          y (range-inclusive y1 y2)]
      [x y])
    (map vector
         (range-inclusive x1 x2)
         (range-inclusive y1 y2))))

(defn count-overlaps [lines]
  (->> (reduce (fn [acc line]
                 (let [points (points-on-line line)]
                   (persistent!
                    (reduce (fn [m point]
                              (assoc! m point (inc (get m point 0))))
                            (transient acc)
                            points))))
               {}
               lines)
       (filter #(>= (val %) 2))
       count))

(defn solve-part-one [lines]
  (->> lines
       (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))))
       count-overlaps))

(defn solve-part-two [lines]
  (count-overlaps lines))

(defn -main []
  (let [lines (->> (slurp "input.txt")
                   str/split-lines
                   (mapv parse-line))]
    (println "Part One:" (solve-part-one lines))
    (println "Part Two:" (solve-part-two lines))))

(-main)
