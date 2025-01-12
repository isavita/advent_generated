
(defn parse-grid [lines]
  (reduce (fn [grid [y line]]
            (reduce (fn [g [x c]]
                      (assoc g [x y] (- (int c) (int \0))))
                    grid
                    (map-indexed vector line)))
          {}
          (map-indexed vector lines)))

(defn neighbors [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn view-distance [grid start dir]
  (loop [pos start dist 0]
    (let [next (mapv + pos dir)]
      (if (contains? grid next)
        (if (>= (get grid next) (get grid start))
          (inc dist)
          (recur next (inc dist)))
        dist))))

(defn scenic-score [grid pos]
  (reduce * 1
          (map (fn [dir] (view-distance grid pos dir))
               [[1 0] [-1 0] [0 1] [0 -1]])))

(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        grid (parse-grid lines)]
    (->> (keys grid)
         (map (partial scenic-score grid))
         (apply max))))

(println (solve))
