
(require '[clojure.string :as str])

(defn parse-input [input]
  (let [lines (str/split-lines input)
        parse-coord (fn [s] (let [[c r] (str/split s #",")]
                              [(Integer/parseInt c) (Integer/parseInt r)]))
        parse-line (fn [line] (map parse-coord (str/split line #" -> ")))
        coord-sets (map parse-line lines)
        all-coords (apply concat coord-sets)
        cols (map first all-coords)
        rows (map second all-coords)
        min-col (apply min cols)
        max-col (apply max cols)
        max-row (apply max rows)
        extra-left 200
        adjusted-min-col (- min-col extra-left)
        adjusted-max-col (+ (- max-col min-col) (* 2 extra-left))
        num-rows (+ max-row 3)
        num-cols (inc adjusted-max-col)
        origin-col (- 500 adjusted-min-col)
        matrix (vec (repeat num-rows (vec (repeat num-cols \.))))
        matrix (reduce
                (fn [m set]
                  (loop [m m, set (seq set)]
                    (if (or (empty? set) (empty? (rest set)))
                      m
                      (let [[[c1 r1] [c2 r2]] [(first set) (second set)]
                            c1-adj (- c1 adjusted-min-col)
                            c2-adj (- c2 adjusted-min-col)
                            min-c (min c1-adj c2-adj)
                            max-c (max c1-adj c2-adj)
                            min-r (min r1 r2)
                            max-r (max r1 r2)
                            m (if (= min-c max-c)
                                (reduce (fn [m r] (assoc-in m [r min-c] \#))
                                        m (range min-r (inc max-r)))
                                (reduce (fn [m c] (assoc-in m [min-r c] \#))
                                        m (range min-c (inc max-c))))]
                        (recur m (rest set))))))
                matrix
                coord-sets)
        matrix (assoc matrix (dec num-rows) (vec (repeat num-cols \#)))
        matrix (assoc-in matrix [0 origin-col] \+)]
    {:matrix matrix :origin-col origin-col}))

(defn drop-sand [matrix origin-col]
  (let [rows (count matrix)]
    (loop [r 0, c origin-col]
      (if (>= r (dec rows))
        [matrix true]
        (let [below (get-in matrix [(inc r) c])
              left (get-in matrix [(inc r) (dec c)])
              right (get-in matrix [(inc r) (inc c)])]
          (cond
            (= below \.) (recur (inc r) c)
            (= left \.) (recur (inc r) (dec c))
            (= right \.) (recur (inc r) (inc c))
            :else [(assoc-in matrix [r c] \o) false]))))))

(defn solve [input]
  (let [{:keys [matrix origin-col]} (parse-input input)]
    (loop [matrix matrix, ans 0]
      (if (= (get-in matrix [0 origin-col]) \o)
        ans
        (let [[new-matrix abyss?] (drop-sand matrix origin-col)]
          (if abyss?
            ans
            (recur new-matrix (inc ans))))))))

(println (solve (slurp "input.txt")))
