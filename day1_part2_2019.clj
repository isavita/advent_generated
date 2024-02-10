
(defn calculate-fuel [mass]
  (let [fuel (max 0 (- (quot mass 3) 2))]
    (if (zero? fuel)
      0
      (+ fuel (calculate-fuel fuel)))))

(defn read-input []
  (slurp "input.txt"))

(defn -main []
  (let [input (read-input)
        masses (mapv #(Integer/parseInt %) (clojure.string/split input #"\n"))
        total-fuel (reduce + (mapv calculate-fuel masses))]
    (println total-fuel)))

(-main)
