
(defn abs [x]
  (if (neg? x) (- x) x))

(defn first-revisited-distance [instructions]
  (loop [pos {:x 0 :y 0}
         visited #{{:x 0 :y 0}}
         directions [{:x 0 :y 1} {:x 1 :y 0} {:x 0 :y -1} {:x -1 :y 0}]
         dir-index 0
         instr instructions]
    (if (empty? instr)
      -1
      (let [turn (subs (first instr) 0 1)
            blocks (Integer/parseInt (subs (first instr) 1))
            new-dir-index (if (= turn "R")
                            (mod (inc dir-index) 4)
                            (mod (dec dir-index) 4))
            [new-pos new-visited]
            (reduce
             (fn [[p v] _]
               (let [next-pos {:x (+ (:x p) (:x (nth directions new-dir-index)))
                               :y (+ (:y p) (:y (nth directions new-dir-index)))}]
                 (if (v next-pos)
                   (reduced [next-pos v])
                   [next-pos (conj v next-pos)])))
             [pos visited]
             (range blocks))]
        (if (visited new-pos)
          (+ (abs (:x new-pos)) (abs (:y new-pos)))
          (recur new-pos new-visited directions new-dir-index (rest instr)))))))

(let [instructions (clojure.string/split (slurp "input.txt") #", ")]
  (println (first-revisited-distance instructions)))
