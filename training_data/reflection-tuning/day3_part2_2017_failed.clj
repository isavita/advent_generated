(defn ring-size [n]
  (let [root (int (Math/ceil (Math/sqrt n)))]
    (if (even? root) (inc root) root)))

(defn manhattan-distance [n]
  (if (= n 1)
    0
    (let [size (ring-size n)
          max-in-ring (* size size)
          side-length (dec size)
          steps-in (- max-in-ring n)
          side (quot steps-in side-length)
          offset (rem steps-in side-length)
          mid (quot side-length 2)]
      (+ (quot size 2) (Math/abs (- mid offset)))))

(defn next-pos [[x y]]
  (cond
    (and (>= x y) (>= (- x) y)) [(dec x) y]
    (and (> y x) (>= y (- x))) [x (dec y)]
    (and (< x y) (< x (- y))) [(inc x) y]
    :else [x (inc y)]))

(defn adjacent-sum [grid [x y]]
  (apply + (for [dx [-1 0 1]
                 dy [-1 0 1]
                 :when (not= [dx dy] [0 0])]
             (get grid [(+ x dx) (+ y dy)] 0))))

(defn first-larger-value [target]
  (loop [grid {[0 0] 1}
         pos [1 0]]
    (let [value (adjacent-sum grid pos)]
      (if (> value target)
        value
        (recur (assoc grid pos value) (next-pos pos))))))

(defn solve [input]
  (let [n (Integer/parseInt input)]
    {:part1 (manhattan-distance n)
     :part2 (first-larger-value n)}))

(println (solve "361527"))
