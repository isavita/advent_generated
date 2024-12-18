
(defn solve-race [time record]
  (count (filter #(> % record)
                 (for [hold-time (range 1 time)]
                   (* hold-time (- time hold-time))))))

(defn solve []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)
          times (map #(Integer/parseInt %) (re-seq #"\d+" (first lines)))
          records (map #(Integer/parseInt %) (re-seq #"\d+" (second lines)))]
      (reduce * (map solve-race times records)))))

(println (solve))
