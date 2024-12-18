
(defn valid-triangle? [a b c]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn solve []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (->> (line-seq rdr)
         (map #(re-seq #"\d+" %))
         (filter (comp (partial = 3) count))
         (map (fn [sides] (map #(Integer/parseInt %) sides)))
         (filter (fn [[a b c]] (valid-triangle? a b c)))
         count)))

(println (solve))
