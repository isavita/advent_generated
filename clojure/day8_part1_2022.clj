
(defn solve []
  (let [grid (->> (slurp "input.txt")
                  (clojure.string/split-lines)
                  (map-indexed (fn [y line]
                                (map-indexed (fn [x c]
                                               [[x y] (- (int c) (int \0))])
                                             line)))
                  (apply concat)
                  (into {}))
        neighbors [[0 1] [0 -1] [1 0] [-1 0]]
        visible (atom #{})]
    (doseq [[p v] grid]
      (doseq [n neighbors]
        (loop [next (mapv + p n)]
          (if (contains? grid next)
            (if (>= (get grid next) v)
              nil
              (recur (mapv + next n)))
            (do (swap! visible conj p) nil)))))
    (count @visible)))

(println (solve))
