(defn calculate-floor [instructions]
  (reduce (fn [floor char]
            (case char
              \( (inc floor)
              \) (dec floor)
              floor))
          0
          instructions))

(defn solve-puzzle []
  (let [input (slurp "input.txt")
        final-floor (calculate-floor input)]
    (println final-floor)))

(solve-puzzle)
