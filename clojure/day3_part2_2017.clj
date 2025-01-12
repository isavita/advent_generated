
(require '[clojure.string :as str])

(defn solve []
  (let [target (-> (slurp "input.txt") str/trim Integer/parseInt)
        grid (atom {[0 0] 1})
        x (atom 0)
        y (atom 0)
        dx (atom 0)
        dy (atom -1)]
    (loop []
      (if (or (= @x @y) (and (< @x 0) (= @x (- @y))) (and (> @x 0) (= @x (inc (- @y)))))
        (let [temp @dx]
          (reset! dx (- @dy))
          (reset! dy temp)))
      (swap! x + @dx)
      (swap! y + @dy)
      (let [value (reduce + (for [i (range -1 2) j (range -1 2)]
                               (get @grid [ (+ @x i) (+ @y j) ] 0)))]
        (swap! grid assoc [@x @y] value)
        (if (> value target)
          (println value)
          (recur))))))

(solve)
