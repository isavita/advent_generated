(require '[clojure.string :as str])

(defn redistribute [banks]
  (let [max-val (apply max banks)
        max-idx (.indexOf banks max-val)
        banks-count (count banks)
        updated-banks (assoc banks max-idx 0)]
    (reduce (fn [acc idx]
              (update-in acc [(mod (+ max-idx 1 idx) banks-count)] + 1))
            updated-banks
            (range max-val))))

(defn solve-part1-and-2 [initial-banks]
  (loop [banks initial-banks
         seen {banks 0}
         cycles 0]
    (let [new-banks (redistribute banks)]
      (if-let [previous-cycle (seen new-banks)]
        [cycles (- cycles previous-cycle)]
        (recur new-banks (assoc seen new-banks cycles) (inc cycles))))))

(let [input (-> (slurp "input.txt")
                str/trim
                (str/split #"\s+")
                (->> (mapv #(Integer/parseInt %))))
      [part1 part2] (solve-part1-and-2 input)]
  (println "Part 1:" part1)
  (println "Part 2:" part2))
