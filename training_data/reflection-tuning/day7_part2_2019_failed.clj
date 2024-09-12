(ns amplification-circuit)

(defn parse-program [input]
  (mapv parse-long (clojure.string/split input #",")))

(defn run-intcode [program inputs]
  (loop [memory program
         pc 0
         inputs inputs
         outputs []]
    (let [opcode (get memory pc)]
      (case (mod opcode 100)
        99 {:outputs outputs :memory memory}
        (1 2 3 4 5 6 7 8)
        (let [[p1 p2 p3] (subvec memory (inc pc) (+ pc 4))
              [m1 m2 m3] [(quot (mod opcode 1000) 100)
                          (quot (mod opcode 10000) 1000)
                          (quot opcode 100000)]
              v1 (if (zero? m1) (get memory p1) p1)
              v2 (if (zero? m2) (get memory p2) p2)]
          (case (mod opcode 100)
            1 (recur (assoc memory p3 (+ v1 v2)) (+ pc 4) inputs outputs)
            2 (recur (assoc memory p3 (* v1 v2)) (+ pc 4) inputs outputs)
            3 (recur (assoc memory p1 (first inputs)) (+ pc 2) (rest inputs) outputs)
            4 (recur memory (+ pc 2) inputs (conj outputs v1))
            5 (recur memory (if (not= 0 v1) v2 (+ pc 3)) inputs outputs)
            6 (recur memory (if (zero? v1) v2 (+ pc 3)) inputs outputs)
            7 (recur (assoc memory p3 (if (< v1 v2) 1 0)) (+ pc 4) inputs outputs)
            8 (recur (assoc memory p3 (if (= v1 v2) 1 0)) (+ pc 4) inputs outputs)))))))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn run-amplifiers [program phase-settings]
  (reduce (fn [signal phase]
            (first (:outputs (run-intcode program [phase signal]))))
          0
          phase-settings))

(defn run-feedback-loop [program phase-settings]
  (loop [amplifiers (mapv (fn [phase] {:program program :phase phase :pc 0 :inputs [phase]}) phase-settings)
         signal 0
         amp-index 0]
    (let [current-amp (get amplifiers amp-index)
          updated-amp (update current-amp :inputs conj signal)
          result (run-intcode (:program updated-amp) (:inputs updated-amp))
          new-signal (first (:outputs result))]
      (if (and (= amp-index 4) (empty? (:outputs result)))
        signal
        (recur (assoc amplifiers amp-index (assoc updated-amp :program (:memory result) :inputs []))
               (or new-signal signal)
               (mod (inc amp-index) 5))))))

(defn solve-part1 [input]
  (let [program (parse-program input)]
    (apply max (map #(run-amplifiers program %) (permutations (range 5))))))

(defn solve-part2 [input]
  (let [program (parse-program input)]
    (apply max (map #(run-feedback-loop program %) (permutations (range 5 10))))))

(defn -main [& args]
  (let [input (slurp (first args))]
    (println "Part 1:" (solve-part1 input))
    (println "Part 2:" (solve-part2 input))))
