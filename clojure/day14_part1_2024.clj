
(defn parse-line [line]
  (let [[p-part v-part] (clojure.string/split line #" ")
        [px py] (map #(Integer/parseInt %) (clojure.string/split (subs p-part 2) #","))
        [vx vy] (map #(Integer/parseInt %) (clojure.string/split (subs v-part 2) #","))]
    [px py vx vy]))

(defn update-robot [[x y vx vy] width height]
  (let [nx (mod (+ x vx) width)
        ny (mod (+ y vy) height)]
    [(if (neg? nx) (+ nx width) nx) (if (neg? ny) (+ ny height) ny) vx vy]))

(defn solve []
  (let [lines (-> "input.txt" slurp clojure.string/split-lines)
        robots (map parse-line lines)
        width 101
        height 103
        updated-robots (->> robots
                            (iterate (fn [rs] (map #(update-robot % width height) rs)))
                            (drop 100)
                            first)]
    (let [quadrants (reduce (fn [counts [x y _ _]]
                             (cond
                               (or (= x 50) (= y 51)) counts
                               (and (< x 50) (< y 51)) (update counts 0 inc)
                               (and (> x 50) (< y 51)) (update counts 1 inc)
                               (and (< x 50) (> y 51)) (update counts 2 inc)
                               (and (> x 50) (> y 51)) (update counts 3 inc)
                               :else counts))
                           [0 0 0 0] updated-robots)]
      (apply * quadrants))))

(println (solve))
