
(defn read-input []
  (Integer/parseInt (clojure.string/trim (slurp "input.txt"))))

(defn power-level [x y serial]
  (let [rack-id (+ x 11)]
    (- (mod (quot (* (+ (* rack-id (+ y 1)) serial) rack-id) 100) 10) 5)))

(defn build-grid [serial]
  (vec (for [y (range 300)]
         (vec (for [x (range 300)]
                (power-level x y serial))))))

(defn build-sum-grid [grid]
  (let [size 300
        sum-grid (vec (repeat (inc size) (vec (repeat (inc size) 0))))]
    (reduce
     (fn [sg y]
       (reduce
        (fn [sg2 x]
          (assoc-in sg2 [(inc y) (inc x)]
                    (+ (get-in grid [y x])
                       (get-in sg2 [(inc y) x])
                       (get-in sg2 [y (inc x)])
                       (- (get-in sg2 [y x])))))
        sg (range size)))
     sum-grid (range size))))

(defn find-max-power [grid]
  (let [size 300
        sum-grid (build-sum-grid grid)]
    (reduce
     (fn [[max-power max-x max-y max-size] s]
       (reduce
        (fn [[mp mx my ms] y]
          (reduce
           (fn [[mp2 mx2 my2 ms2] x]
             (let [total (- (+ (get-in sum-grid [(+ y s) (+ x s)])
                               (get-in sum-grid [y x]))
                            (+ (get-in sum-grid [y (+ x s)])
                               (get-in sum-grid [(+ y s) x])))]
               (if (> total mp2)
                 [total (inc x) (inc y) s]
                 [mp2 mx2 my2 ms2])))
           [mp mx my ms] (range (- size s))))
        [max-power max-x max-y max-size] (range (- size s))))
     [Integer/MIN_VALUE 0 0 0] (range 1 (inc size)))))

(defn solve []
  (let [serial (read-input)
        grid (build-grid serial)
        [max-power x y size] (find-max-power grid)]
    (println (str x "," y "," size))))

(solve)
