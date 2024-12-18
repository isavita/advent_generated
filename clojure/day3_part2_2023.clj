
(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        grid (into {} (for [y (range (count lines))
                            x (range (count (nth lines y)))
                            :let [c (-> lines (nth y) (nth x))]]
                        {[x y] c}))
        parts (loop [y 0 x 0 curr nil parts []]
                (if (>= y (count lines))
                  (if curr (conj parts curr) parts)
                  (let [line (nth lines y)
                        c (if (< x (count line)) (nth line x) nil)]
                    (cond
                      (nil? c) (recur (inc y) 0 nil (if curr (conj parts curr) parts))
                      (Character/isDigit c) (if curr
                                              (recur y (inc x) (assoc curr :xmax x :n (+ (* (:n curr) 10) (- (int c) (int \0)))) parts)
                                              (recur y (inc x) {:xmin x :xmax x :y y :n (- (int c) (int \0))} parts))
                      :else (recur y (inc x) nil (if curr (conj parts curr) parts))))))]
    {:grid grid :parts parts}))

(defn valid? [grid part]
  (let [neighbors [[0 1] [0 -1] [1 0] [-1 0] [-1 -1] [-1 1] [1 -1] [1 1]]]
    (some (fn [x]
            (some (fn [[dx dy]]
                    (let [c (get grid [(+ x (:y part)) (+ dx (:y part))])]
                      (and c (not= c \.) (not (Character/isDigit c)))))
                  neighbors))
          (range (:xmin part) (inc (:xmax part))))))

(defn solve []
  (let [{:keys [grid parts]} (-> "input.txt" slurp parse-input)
        parts-grid (reduce (fn [m i]
                             (let [part (nth parts i)]
                               (reduce (fn [m x] (assoc m [x (:y part)] i)) m (range (:xmin part) (inc (:xmax part))))))
                           {} (range (count parts)))
        neighbors [[0 1] [0 -1] [1 0] [-1 0] [-1 -1] [-1 1] [1 -1] [1 1]]]
    (reduce (fn [sum [p c]]
              (if (= c \*)
                (let [neighbor-parts (->> neighbors
                                          (map (fn [[dx dy]] (get parts-grid [(+ (first p) dx) (+ (second p) dy)])))
                                          (remove nil?)
                                          (distinct)
                                          (count))]
                  (if (= neighbor-parts 2)
                    (let [part-indices (->> neighbors
                                            (map (fn [[dx dy]] (get parts-grid [(+ (first p) dx) (+ (second p) dy)])))
                                            (remove nil?)
                                            (distinct))]
                      (+ sum (reduce * (map (fn [i] (:n (nth parts i))) part-indices))))
                    sum))
                sum))
            0 grid)))

(println (solve))
