
(defn parse-input [lines]
  (let [grid (mapv vec lines)]
    (reduce (fn [acc [y row]]
              (reduce (fn [acc [x char]]
                        (if (or (Character/isLetterOrDigit char))
                          (update acc char conj [x y])
                          acc))
                      acc (map-indexed vector row)))
            {} (map-indexed vector grid))))

(defn antinodes [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        dx (- x2 x1)
        dy (- y2 y1)]
    [[(+ x2 dx) (+ y2 dy)]
     [(- x1 dx) (- y1 dy)]]))

(defn solve []
  (let [lines (line-seq (java.io.BufferedReader. (java.io.FileReader. "input.txt")))
        antenna-map (parse-input lines)
        width (count (first lines))
        height (count lines)]
    (->> antenna-map
         (mapcat (fn [[_ positions]]
                   (for [i (range (count positions))
                         j (range (inc i) (count positions))]
                     (antinodes (nth positions i) (nth positions j)))))
         (mapcat identity)
         (filter (fn [[x y]] (and (>= x 0) (< x width) (>= y 0) (< y height))))
         set
         count
         println)))

(solve)
