
(require '[clojure.string :as str])

(defn read-points []
  (->> (slurp "input.txt")
       str/split-lines
       (map (fn [line]
              (let [[x y z] (str/split line #",")]
                [(Integer/parseInt x) (Integer/parseInt y) (Integer/parseInt z)])))
       vec))

(defn make-edges [points]
  (let [n (count points)]
    (for [i (range n)
          j (range (inc i) n)
          :let [[x1 y1 z1] (points i)
                [x2 y2 z2] (points j)
                dx (- x1 x2)
                dy (- y1 y2)
                dz (- z1 z2)
                d (+ (* dx dx) (* dy dy) (* dz dz))]]
      {:u i :v j :d d})))

(defn find-set [parent x]
  (if (= (parent x) x)
    x
    (recur parent (parent x))))

(defn union-set [parent size a b]
  (let [ra (find-set parent a)
        rb (find-set parent b)]
    (when-not (= ra rb)
      (let [[ra rb] (if (< (size ra) (size rb)) [rb ra] [ra rb])]
        [(assoc parent rb ra)
         (update size ra + (size rb))]))))

(defn solve []
  (let [points (read-points)
        n (count points)
        edges (->> (make-edges points)
                   (sort-by :d)
                   (take 1000))
        parent (vec (range n))
        size (vec (repeat n 1))
        [final-parent final-size]
        (reduce (fn [[p s] {:keys [u v]}]
                  (if-let [[new-p new-s] (union-set p s u v)]
                    [new-p new-s]
                    [p s]))
                [parent size]
                edges)
        roots (set (range n))
        component-sizes (->> roots
                            (map #(find-set final-parent %))
                            frequencies
                            vals
                            sort
                            reverse
                            (take 3))]
    (reduce * component-sizes)))

(println (solve))
