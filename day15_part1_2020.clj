
(defn play-game [starting-numbers n]
  (let [spoken-numbers (vec starting-numbers)
        spoken-map (zipmap starting-numbers (range 1 (count starting-numbers)))
        last-spoken (peek starting-numbers)]
    (loop [turn (count starting-numbers)
           last-spoken last-spoken
           spoken-numbers spoken-numbers
           spoken-map spoken-map]
      (if (= turn n)
        last-spoken
        (let [next-spoken (if (contains? spoken-map last-spoken)
                            (- turn (get spoken-map last-spoken))
                            0)]
          (recur (inc turn)
                 next-spoken
                 (conj spoken-numbers next-spoken)
                 (assoc spoken-map last-spoken turn)))))))

(def input (slurp "input.txt"))
(def starting-numbers (mapv #(Integer/parseInt %) (clojure.string/split input #",")))

(println (play-game starting-numbers 2020))
