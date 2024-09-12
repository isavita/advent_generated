(ns amplification-circuit)

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (remove #{head} colls))]
      (cons head tail))))

(defn parse-program [input]
  (mapv read-string (clojure.string/split input #",")))

(defn run-intcode [program inputs]
  (loop [memory program
         pc 0
         inputs inputs
         outputs []]
    (let [opcode (get memory pc)]
      (case opcode
        99 {:outputs outputs :memory memory}
        1 (recur (assoc memory (get memory (+ pc 3))
                        (+ (get memory (get memory (+ pc 1)))
                           (get memory (get memory (+ pc 2)))))
                 (+ pc 4) inputs outputs)
        2 (recur (assoc memory (get memory (+ pc 3))
                        (* (get memory (get memory (+ pc 1)))
                           (get memory (get memory (+ pc 2)))))
                 (+ pc 4) inputs outputs)
        3 (if (empty? inputs)
            {:outputs outputs :memory memory :pc pc :waiting-for-input true}
            (recur (assoc memory (get memory (+ pc 1)) (first inputs))
                   (+ pc 2) (rest inputs) outputs))
        4 (recur memory (+ pc 2) inputs
                 (conj outputs (get memory (get memory (+ pc 1)))))
        (throw (Exception. (str "Unknown opcode: " opcode)))))))

(defn run-amplifier-sequence [program phase-settings]
  (loop [amplifiers (vec (repeat 5 {:program program :pc 0 :memory program}))
         input 0
         phase-settings phase-settings]
    (if (empty? phase-settings)
      (loop [amplifiers amplifiers
             amp-index 0
             input input]
        (let [amp (get amplifiers amp-index)
              result (run-intcode (:memory amp) (if (= amp-index 0) [input] [input]))]
          (if (:waiting-for-input result)
            input
            (let [output (first (:outputs result))
                  new-amp (assoc amp :memory (:memory result) :pc (:pc result))
                  new-amplifiers (assoc amplifiers amp-index new-amp)]
              (if (and (= amp-index 4) (not (:waiting-for-input result)))
                (recur new-amplifiers 0 output)
                (recur new-amplifiers (mod (inc amp-index) 5) output))))))
      (let [amp (first amplifiers)
            result (run-intcode (:program amp) [(first phase-settings) input])
            output (first (:outputs result))
            new-amp (assoc amp :memory (:memory result) :pc (:pc result))
            new-amplifiers (conj (vec (rest amplifiers)) new-amp)]
        (recur new-amplifiers output (rest phase-settings))))))

(defn solve [input]
  (let [program (parse-program input)
        phase-settings (permutations (range 5))]
    (apply max (map #(run-amplifier-sequence program %) phase-settings))))

;; Example usage:
(def example-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(println (solve example-input))  ; Should print 43210
