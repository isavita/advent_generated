
(defn parse-instruction [line]
  (let [[cmd x y] (clojure.string/split line #" ")]
    {:cmd cmd :x x :y y}))

(defn get-value [registers s]
  (if (re-matches #"-?\d+" s)
    (Integer/parseInt s)
    (get registers s 0)))

(defn execute [registers pointer instructions mul-count]
  (if (or (neg? pointer) (>= pointer (count instructions)))
    mul-count
    (let [{:keys [cmd x y]} (nth instructions pointer)
          value-y (get-value registers y)]
      (case cmd
        "set" (recur (assoc registers x value-y) (inc pointer) instructions mul-count)
        "sub" (recur (assoc registers x (- (get-value registers x) value-y)) (inc pointer) instructions mul-count)
        "mul" (recur (assoc registers x (* (get-value registers x) value-y)) (inc pointer) instructions (inc mul-count))
        "jnz" (if (not= 0 (get-value registers x))
                (recur registers (+ pointer value-y) instructions mul-count)
                (recur registers (inc pointer) instructions mul-count))))))

(defn solve []
  (let [instructions (-> "input.txt"
                         slurp
                         (clojure.string/split-lines)
                         (->> (map parse-instruction)))]
    (execute {} 0 instructions 0)))

(println (solve))
