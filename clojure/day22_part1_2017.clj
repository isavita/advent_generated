
(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        start-y (quot (count lines) 2)
        start-x (quot (count (first lines)) 2)
        initial-grid (reduce (fn [grid [y line]]
                              (reduce (fn [g [x c]]
                                        (if (= c \#)
                                          (assoc g [x y] true)
                                          g))
                                      grid
                                      (map-indexed vector line)))
                            {}
                            (map-indexed vector lines))
        dx [0 1 0 -1]
        dy [-1 0 1 0]]
    (loop [grid initial-grid
           x start-x
           y start-y
           dir 0
           infected-count 0
           i 0]
      (if (= i 10000)
        infected-count
        (let [pos [x y]
              infected (get grid pos)
              [new-dir new-grid new-infected-count]
              (if infected
                [(mod (inc dir) 4) (dissoc grid pos) infected-count]
                [(mod (- dir 1) 4) (assoc grid pos true) (inc infected-count)])
              new-x (+ x (nth dx new-dir))
              new-y (+ y (nth dy new-dir))]
          (recur new-grid new-x new-y new-dir new-infected-count (inc i)))))))

(println (solve))
