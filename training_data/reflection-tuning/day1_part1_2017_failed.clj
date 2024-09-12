(defn solve-captcha [input]
  (let [digits (map #(Character/digit % 10) input)
        circular-digits (concat digits [(first digits)])]
    (->> (map vector digits circular-digits)
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))

(defn -main []
  (let [input (clojure.string/trim (slurp "input.txt"))
        result (solve-captcha input)]
    (println result)))

(-main)
