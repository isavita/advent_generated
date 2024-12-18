
(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        start-x (quot (count (first lines)) 2)
        start-y (quot (count lines) 2)
        initial-grid (reduce (fn [grid [y line]]
                               (reduce (fn [g [x c]]
                                         (if (= c \#)
                                           (assoc g [x y] 2)
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
      (if (>= i 10000000)
        infected-count
        (let [pos [x y]
              state (get grid pos 0)
              [new-dir new-state new-infected-count]
              (case state
                0 [(mod (- dir 1) 4) 1 infected-count]
                1 [dir 2 (inc infected-count)]
                2 [(mod (inc dir) 4) 3 infected-count]
                3 [(mod (+ dir 2) 4) 0 infected-count])]
          (recur (assoc grid pos new-state)
                 (+ x (nth dx new-dir))
                 (+ y (nth dy new-dir))
                 new-dir
                 new-infected-count
                 (inc i)))))))

(println (solve))
