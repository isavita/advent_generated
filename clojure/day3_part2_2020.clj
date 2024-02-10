
(defn count-trees [slope-right slope-down grid]
  (loop [x 0
         y 0
         trees 0]
    (if (>= y (count grid))
      trees
      (recur (+ x slope-right)
             (+ y slope-down)
             (+ trees (if (= (get-in grid [y (mod x (count (first grid)))]) \#) 1 0))))))

(defn main []
  (let [input (slurp "input.txt")
        grid (mapv vec (clojure.string/split-lines input))]
    (println (* (count-trees 1 1 grid)
                (count-trees 3 1 grid)
                (count-trees 5 1 grid)
                (count-trees 7 1 grid)
                (count-trees 1 2 grid)))))

(main)
