(require '[clojure.string :as str])

(defn redistribute [banks]
  (let [max-val (apply max banks)
        max-idx (.indexOf banks max-val)
        bank-count (count banks)
        new-banks (assoc banks max-idx 0)]
    (loop [b new-banks
           blocks max-val
           idx (mod (inc max-idx) bank-count)]
      (if (zero? blocks)
        b
        (recur (update b idx inc)
               (dec blocks)
               (mod (inc idx) bank-count))))))

(defn solve [input]
  (loop [banks input
         seen #{banks}
         cycles 0]
    (let [new-banks (redistribute banks)]
      (if (seen new-banks)
        (inc cycles)
        (recur new-banks
               (conj seen new-banks)
               (inc cycles))))))

(defn -main []
  (let [input (->> (slurp "input.txt")
                   str/trim
                   (#(str/split % #"\s+"))
                   (mapv #(Integer/parseInt %)))]
    (println (solve input))))

(-main)
