
(require '[clojure.string :as str])

(defn parse-path [path]
  (map (fn [m] (if (re-matches #"\d+" m) {:steps (Integer/parseInt m)} {:rotate m}))
       (re-seq #"\d+|[LR]" path)))

(defn cross-border [x y dir size]
  (cond
    (and (= x -1) (< y (* 2 size))) {:curr [(+ y (* 2 size)) 0] :facing 1}
    (and (= x -1) (>= y (* 2 size))) {:curr [(dec (* 4 size)) (- y (* 2 size))] :facing 0}
    (and (= x size) (= dir 2)) {:curr [(- y size) (dec (* 2 size))] :facing 3}
    (and (= x (dec (* 2 size))) (= dir 0)) {:curr [(+ y size) size] :facing 1}
    (and (= x (* 3 size)) (= dir 2)) {:curr [(+ y (* 2 size)) (dec size)] :facing 3}
    (and (= x (* 4 size))) {:curr [0 (+ y (* 2 size))] :facing 2}
    (and (= y -1) (< x (* 3 size))) {:curr [(- (dec (* 3 size)) x) size] :facing 1}
    (and (= y -1) (>= x (* 3 size))) {:curr [0 (- x (* 2 size))] :facing 2}
    (and (= y (dec size)) (< x size)) {:curr [(- (dec (* 3 size)) x) 0] :facing 1}
    (and (= y (dec size)) (>= x size) (< x (* 2 size)) (= dir 3)) {:curr [(* 2 size) (- x size)] :facing 2}
    (and (= y size) (= dir 1)) {:curr [(dec (* 3 size)) (- x (* 2 size))] :facing 0}
    (and (= y (* 2 size)) (< x (* 2 size)) (= dir 1)) {:curr [(dec size) (+ x size)] :facing 0}
    (and (= y (* 2 size)) (>= x (* 2 size))) {:curr [(- (dec (* 3 size)) x) (dec (* 3 size))] :facing 3}
    (and (= y (* 3 size))) {:curr [(- (dec (* 3 size)) x) (dec (* 2 size))] :facing 3}))

(defn walk [state grid size]
  (let [[x y] (:curr state) dir (:facing state)
        [dx dy] (nth [[-1 0] [0 1] [1 0] [0 -1]] dir)
        nx (+ x dx) ny (+ y dy)]
    (if (contains? grid [nx ny])
      (if (get grid [nx ny]) state (assoc state :curr [nx ny]))
      (let [{:keys [curr facing]} (cross-border nx ny dir size)]
        (if (get grid curr) state {:curr curr :facing facing})))))

(defn solve []
  (let [lines (str/split-lines (slurp "input.txt"))
        empty-idx (.indexOf lines "")
        map-lines (take empty-idx lines)
        path-line (str/join "" (map str/trim (drop (inc empty-idx) lines)))
        size (quot (count (first map-lines)) 3)
        grid (into {} (for [[r line] (map-indexed vector map-lines)
                            [c ch] (map-indexed vector line)
                            :when (or (= ch \#) (= ch \.))]
                        [[r c] (= ch \#)]))
        movements (parse-path path-line)
        final (reduce (fn [state mov]
                        (if (:rotate mov)
                          (update state :facing #(if (= (:rotate mov) "R") (mod (inc %) 4) (mod (dec %) 4)))
                          (reduce (fn [s _] (walk s grid size)) state (range (:steps mov)))))
                      {:curr [0 size] :facing 1} movements)
        [r c] (:curr final) f (:facing final)]
    (println (+ (* 1000 (inc r)) (* 4 (inc c)) (mod (+ f 3) 4)))))

(solve)

