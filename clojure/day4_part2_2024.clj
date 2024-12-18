
(defn check-mas [grid x y dx dy]
  (let [word "MAS"
        forward (loop [i 0]
                  (if (= i (count word))
                    true
                    (let [nx (+ x (* dx i))
                          ny (+ y (* dy i))]
                      (if (or (< nx 0) (< ny 0) (>= nx (count grid)) (>= ny (count (first grid))))
                        false
                        (if (not= (get-in grid [nx ny]) (nth word i))
                          false
                          (recur (inc i)))))))
        backward (loop [i 0]
                   (if (= i (count word))
                     true
                     (let [nx (+ x (* dx i))
                           ny (+ y (* dy i))]
                       (if (or (< nx 0) (< ny 0) (>= nx (count grid)) (>= ny (count (first grid))))
                         false
                         (if (not= (get-in grid [nx ny]) (nth word (- (count word) 1 i)))
                           false
                           (recur (inc i)))))))]
    (or forward backward)))

(defn check-xmas [grid x y]
  (or (and (check-mas grid (dec x) (dec y) 1 1)
           (check-mas grid (dec x) (inc y) 1 -1))
      (and (check-mas grid (inc x) (dec y) -1 1)
           (check-mas grid (inc x) (inc y) -1 -1))))

(defn count-xmas-patterns [grid]
  (if (or (< (count grid) 3) (< (count (first grid)) 3))
    0
    (reduce + (for [i (range 1 (dec (count grid)))
                    j (range 1 (dec (count (first grid))))
                    :when (= (get-in grid [i j]) \A)
                    :when (check-xmas grid i j)]
                1))))

(defn solve []
  (let [grid (->> "input.txt"
                  slurp
                  (clojure.string/split-lines)
                  (remove clojure.string/blank?)
                  vec)]
    (printf "X-MAS patterns appear %d times in the word search\n" (count-xmas-patterns grid))))

(solve)
