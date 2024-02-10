
(defn checksum [input]
  (apply + (map #(- (apply max %) (apply min %)) input)))

(defn evenly-divisible-sum [input]
  (apply + (map #(apply / (first (for [x % y % :when (and (not= x y) (zero? (mod x y)))] [x y]))) input)))

(defn parse-input []
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (map #(map read-string (clojure.string/split % #"\s+")))))

(def input (parse-input))
(prn (checksum input))
(prn (evenly-divisible-sum input))
