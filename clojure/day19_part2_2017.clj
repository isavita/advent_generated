
(defn solve []
  (let [grid (vec (map vec (line-seq (java.io.BufferedReader. (java.io.FileReader. "input.txt")))))
        start-x (first (keep-indexed (fn [i c] (when (= c \|) i)) (first grid)))
        walk (fn walk [x y dx dy steps]
               (if (or (< x 0) (>= x (count (first grid))) (< y 0) (>= y (count grid)))
                 steps
                 (let [cell (get-in grid [y x])]
                   (if (= cell \space)
                     steps
                     (let [next-dx-dy (if (= cell \+)
                                        (if (= dx 0)
                                          (if (and (> x 0) (contains? #{\- \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} (get-in grid [y (dec x)])))
                                            [-1 0]
                                            [1 0])
                                          (if (and (> y 0) (contains? #{\| \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} (get-in grid [(dec y) x])))
                                            [0 -1]
                                            [0 1]))
                                        [dx dy])]
                       (recur (+ x (first next-dx-dy)) (+ y (second next-dx-dy)) (first next-dx-dy) (second next-dx-dy) (inc steps)))))))]
    (walk start-x 0 0 1 0)))

(println (solve))
