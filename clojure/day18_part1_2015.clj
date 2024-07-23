
(def grid-size 100)
(def steps 100)

(defn count-on-neighbors [grid x y]
  (reduce
    (fn [on [dx dy]]
      (let [nx (+ x dx) ny (+ y dy)]
        (if (and (>= nx 0) (< nx grid-size) (>= ny 0) (< ny grid-size) (get-in grid [nx ny]))
          (inc on)
          on)))
    0 (for [dx [-1 0 1] dy [-1 0 1] :when (not= dx 0 dy 0)] [dx dy])))

(defn step [grid]
  (vec (for [x (range grid-size)]
         (vec (for [y (range grid-size)]
                (let [on-neighbors (count-on-neighbors grid x y)]
                  (cond
                    (get-in grid [x y]) (or (= on-neighbors 2) (= on-neighbors 3))
                    :else (= on-neighbors 3))))))))

(defn read-input [filename]
  (vec (for [line (line-seq (clojure.java.io/reader filename))]
         (vec (map #(= \# %) line)))))

(defn count-on [grid]
  (reduce + (map #(count (filter identity %)) grid)))

(defn -main []
  (let [grid (read-input "input.txt")]
    (loop [current-grid grid, n steps]
      (if (zero? n)
        (println (count-on current-grid))
        (recur (step current-grid) (dec n))))))

(-main)
