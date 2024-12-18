
(defn count-on-neighbors [grid x y]
  (let [size (count grid)]
    (->> (for [dx (range -1 2) dy (range -1 2)
               :when (not (and (= dx 0) (= dy 0)))]
           (let [nx (+ x dx) ny (+ y dy)]
             (and (>= nx 0) (< nx size) (>= ny 0) (< ny size) (get-in grid [nx ny]))))
         (filter identity)
         count)))

(defn step [grid]
  (let [size (count grid)
        new-grid (vec (for [x (range size)]
                        (vec (for [y (range size)]
                               (let [on-neighbors (count-on-neighbors grid x y)]
                                 (if (get-in grid [x y])
                                   (or (= on-neighbors 2) (= on-neighbors 3))
                                   (= on-neighbors 3)))))))]
    (-> new-grid
        (assoc-in [0 0] true)
        (assoc-in [0 (dec size)] true)
        (assoc-in [(dec size) 0] true)
        (assoc-in [(dec size) (dec size)] true))))

(defn solve []
  (let [grid (->> (slurp "input.txt")
                  (clojure.string/split-lines)
                  (mapv (fn [line] (mapv #(= % \#) line))))
        size (count grid)
        initial-grid (-> grid
                         (assoc-in [0 0] true)
                         (assoc-in [0 (dec size)] true)
                         (assoc-in [(dec size) 0] true)
                         (assoc-in [(dec size) (dec size)] true))
        final-grid (nth (iterate step initial-grid) 100)]
    (->> final-grid
         (mapcat identity)
         (filter identity)
         count)))

(println (solve))
