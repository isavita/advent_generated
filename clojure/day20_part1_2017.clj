
(defn abs [x] (if (neg? x) (- x) x))
(defn manhattan [[x y z]] (+ (abs x) (abs y) (abs z)))

(defn parse-particle [line]
  (let [[p v a] (map #(re-seq #"-?\d+" %) (clojure.string/split line #", "))]
    {:p (mapv #(Integer/parseInt %) p)
     :v (mapv #(Integer/parseInt %) v)
     :a (mapv #(Integer/parseInt %) a)}))

(defn solve []
  (let [particles (->> "input.txt"
                       slurp
                       (clojure.string/split-lines)
                       (map parse-particle)
                       vec)]
    (->> (range (count particles))
         (reduce (fn [closest i]
                   (let [p (nth particles i)
                         accel (manhattan (:a p))
                         velocity (manhattan (:v p))
                         position (manhattan (:p p))
                         closest-p (nth particles closest)
                         closest-accel (manhattan (:a closest-p))
                         closest-velocity (manhattan (:v closest-p))
                         closest-position (manhattan (:p closest-p))]
                     (if (or (< accel closest-accel)
                             (and (= accel closest-accel) (< velocity closest-velocity))
                             (and (= accel closest-accel) (= velocity closest-velocity) (< position closest-position)))
                       i
                       closest)))
                 0)
         println)))

(solve)
