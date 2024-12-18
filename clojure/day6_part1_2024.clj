
(defn solve []
  (let [grid (->> "input.txt"
                  slurp
                  (clojure.string/split-lines)
                  (mapv vec))
        h (count grid)
        w (count (first grid))
        dirs [[0 -1] [1 0] [0 1] [-1 0]]
        start (first (for [y (range h) x (range w)
                           :let [c (get-in grid [y x])]
                           :when (contains? #{\^ \> \v \<} c)]
                       [x y c]))
        [x y c] start
        dir-idx (case c
                  \^ 0
                  \> 1
                  \v 2
                  \< 3)
        visited (volatile! #{[x y]})]
    (loop [x x y y dir-idx dir-idx]
      (let [[dir-x dir-y] (nth dirs dir-idx)
            nx (+ x dir-x)
            ny (+ y dir-y)]
        (if (or (< nx 0) (>= nx w) (< ny 0) (>= ny h))
          (count @visited)
          (if (= \# (get-in grid [ny nx]))
            (recur x y (mod (inc dir-idx) 4))
            (do
              (vswap! visited conj [nx ny])
              (recur nx ny dir-idx))))))))

(println (solve))
