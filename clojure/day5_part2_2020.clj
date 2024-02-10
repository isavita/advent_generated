
(ns solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn binary-to-int [binary-str]
  (reduce (fn [result [i char]]
            (if (= char \1)
              (bit-or result (bit-shift-left 1 (- (count binary-str) i 1)))
              result))
          0
          (map-indexed vector binary-str)))

(defn decode [pass]
  (let [row (binary-to-int (subs pass 0 7))
        column (binary-to-int (subs pass 7))]
    (+ (* row 8) column)))

(let [file (io/reader "input.txt")
      lines (line-seq file)
      seat-ids (map #(decode (str/replace (str/replace (str/replace (str/replace % "F" "0") "B" "1") "L" "0") "R" "1")) lines)
      sorted-seat-ids (sort seat-ids)]
  (loop [i 0]
    (if (< i (dec (count sorted-seat-ids)))
      (if (not= (+ (nth sorted-seat-ids i) 1) (nth sorted-seat-ids (inc i)))
        (println (+ (nth sorted-seat-ids i) 1))
        (recur (inc i))))))
