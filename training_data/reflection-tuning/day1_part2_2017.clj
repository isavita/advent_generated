(defn solve-part1 [digits]
  (let [paired-digits (map vector digits (concat (rest digits) [(first digits)]))]
    (->> paired-digits
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))

(defn solve-part2 [digits]
  (let [len (count digits)
        half-len (/ len 2)
        paired-digits (map vector digits (concat (drop half-len digits) (take half-len digits)))]
    (->> paired-digits
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))

(defn -main []
  (let [input (slurp "input.txt")
        digits (map #(Character/digit % 10) input)]
    (println "Part 1:" (solve-part1 digits))
    (println "Part 2:" (solve-part2 digits))))

(-main)
