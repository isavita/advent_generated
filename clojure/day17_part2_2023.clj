
(import '[java.util PriorityQueue HashSet])

(defn solve [grid min-s max-s]
  (let [h (count grid)
        w (count (first grid))
        pq (PriorityQueue. 1000 (fn [a b] (compare (first a) (first b))))
        visited (HashSet.)
        dirs {:N [0 -1] :S [0 1] :E [1 0] :W [-1 0]}
        turns {:N [:W :E] :S [:E :W] :E [:N :S] :W [:S :N]}]
    (.add pq [0 0 0 :E 0])
    (.add pq [0 0 0 :S 0])
    (loop []
      (if-let [[hl x y dir steps] (.poll pq)]
        (if (and (= x (dec w)) (= y (dec h)) (>= steps min-s))
          hl
          (if (.contains visited [x y dir steps])
            (recur)
            (do
              (.add visited [x y dir steps])
              (when (< steps max-s)
                (let [[dx dy] (dirs dir) nx (+ x dx) ny (+ y dy)]
                  (when (and (< -1 nx w) (< -1 ny h))
                    (.add pq [(+ hl (nth (nth grid ny) nx)) nx ny dir (inc steps)]))))
              (when (>= steps min-s)
                (doseq [nd (turns dir)]
                  (let [[dx dy] (dirs nd) nx (+ x dx) ny (+ y dy)]
                    (when (and (< -1 nx w) (< -1 ny h))
                      (.add pq [(+ hl (nth (nth grid ny) nx)) nx ny nd 1])))))
              (recur))))))))

(let [input (slurp "input.txt")
      grid (vec (map (fn [line] (vec (map #(- (int %) 48) line))) (clojure.string/split-lines input)))]
  (println (solve grid 1 3))
  (println (solve grid 4 10)))
