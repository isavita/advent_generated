(require '[clojure.string :as str])

(defn manhattan-distance [n]
  (if (= n 1)
    0
    (let [root (int (Math/ceil (Math/sqrt n)))
          side (if (even? root) (inc root) root)
          max-in-ring (* side side)
          dist-from-mid (mod (- max-in-ring n) (dec side))]
      (+ (quot (dec side) 2)
         (Math/abs (- dist-from-mid (quot (dec side) 2)))))))

(defn adjacent-coords [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= dx dy 0)]
    [(+ x dx) (+ y dy)]))

(defn next-pos [[x y]]
  (cond
    (and (>= x y) (>= x (- y))) [(inc x) y]
    (and (> y x) (>= y (- x))) [x (dec y)]
    (and (< x y) (<= x (- y))) [(dec x) y]
    :else [x (inc y)]))

(defn spiral-sum [limit]
  (loop [pos [0 0]
         values {[0 0] 1}]
    (let [next-val (apply + (keep values (adjacent-coords pos)))]
      (if (> next-val limit)
        next-val
        (recur (next-pos pos) (assoc values pos next-val))))))

(defn solve-puzzle [input]
  (let [n (Integer/parseInt input)]
    (println "Part One:" (manhattan-distance n))
    (println "Part Two:" (spiral-sum n))))

(defn -main []
  (let [input (str/trim (slurp "input.txt"))]
    (solve-puzzle input)))

(-main)
