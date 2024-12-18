
(defn solve []
  (let [starting-numbers (-> (slurp "input.txt")
                             (clojure.string/trim)
                             (clojure.string/split #","))
        spoken (reduce (fn [m [i num]]
                         (if (= i (dec (count starting-numbers)))
                           m
                           (assoc m (Integer/parseInt num) (inc i))))
                       {}
                       (map-indexed vector starting-numbers))
        last-spoken (Integer/parseInt (last starting-numbers))]
    (loop [turn (inc (count starting-numbers))
           spoken spoken
           last-spoken last-spoken]
      (if (> turn 30000000)
        last-spoken
        (let [next-number (if (contains? spoken last-spoken)
                            (- (dec turn) (get spoken last-spoken))
                            0)
              updated-spoken (assoc spoken last-spoken (dec turn))]
          (recur (inc turn) updated-spoken next-number))))))

(println (solve))
