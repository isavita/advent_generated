(defn fully-contains? [a1 a2 b1 b2]
  (or (<= a1 b1 b2 a2)
      (<= b1 a1 a2 b2)))

(defn parse-range [s]
  (map #(Integer/parseInt %) (clojure.string/split s #"-")))

(defn process-line [line]
  (let [[range1 range2] (clojure.string/split line #",")
        [a1 a2] (parse-range range1)
        [b1 b2] (parse-range range2)]
    (fully-contains? a1 a2 b1 b2)))

(defn solve-puzzle []
  (->> (slurp "input.txt")
       clojure.string/split-lines
       (filter process-line)
       count))

(println (solve-puzzle))
