(ns day5
  (:require [clojure.string :as str]))

(defn char-to-binary [c]
  (case c
    (\F \L) \0
    (\B \R) \1))

(defn boarding-pass-to-seat-id [pass]
  (let [binary-string (apply str (map char-to-binary pass))
        row (Integer/parseInt (subs binary-string 0 7) 2)
        col (Integer/parseInt (subs binary-string 7) 2)]
    (+ (* row 8) col)))

(defn solve-part1 [input]
  (->> input
       str/split-lines
       (map boarding-pass-to-seat-id)
       (apply max)))

(defn solve-part2 [input]
  (let [seat-ids (->> input
                      str/split-lines
                      (map boarding-pass-to-seat-id)
                      sort)
        [min-id max-id] ((juxt first last) seat-ids)
        all-seats (set (range min-id (inc max-id)))]
    (first (clojure.set/difference all-seats (set seat-ids)))))

(defn solve [input]
  {:part1 (solve-part1 input)
   :part2 (solve-part2 input)})
