
(defn calculate-fuel [mass]
  (- (quot mass 3) 2))

(defn read-input []
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (map read-string)))

(defn total-fuel []
  (->> (read-input)
       (map calculate-fuel)
       (reduce +)))

(println (total-fuel))
