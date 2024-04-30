(defn power-level [x y serial-number]
  (let [rack-id (+ x 10)
        power-level (* rack-id y)
        power-level (+ power-level serial-number)
        power-level (* power-level rack-id)
        power-level (quot (mod power-level 1000) 100)
        power-level (- power-level 5)]
    power-level))

(defn total-power [serial-number x y]
  (apply + (for [i (range 3)
                 j (range 3)]
             (power-level (+ x i) (+ y j) serial-number))))

(defn max-total-power [serial-number]
  (apply max-key second (for [x (range 298)
                              y (range 298)]
                          [(str x "," y) (total-power serial-number x y)])))

(defn -main []
  (let [serial-number (Integer/parseInt (slurp "input.txt"))
        [coords max-power] (max-total-power serial-number)]
    (println coords)))

(-main)