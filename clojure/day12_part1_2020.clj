
(defn process-instruction [ship [action value]]
  (case action
    \N (update ship :y + value)
    \S (update ship :y - value)
    \E (update ship :x + value)
    \W (update ship :x - value)
    \L (update ship :facing #(mod (- % value) 360))
    \R (update ship :facing #(mod (+ % value) 360))
    \F (let [facing (:facing ship)]
         (case facing
           0 (update ship :x + value)
           90 (update ship :y - value)
           180 (update ship :x - value)
           270 (update ship :y + value)))))

(defn manhattan-distance [ship]
  (+ (Math/abs (:x ship)) (Math/abs (:y ship))))

(defn main []
  (let [instructions (map #(vector (first %) (Integer/parseInt (subs % 1)))
                           (line-seq (clojure.java.io/reader "input.txt")))
        initial-ship {:x 0 :y 0 :facing 0}]
    (->> instructions
         (reduce process-instruction initial-ship)
         manhattan-distance
         println)))

(main)
