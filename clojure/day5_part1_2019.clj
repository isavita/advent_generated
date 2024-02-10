
(defn get-param [mem idx mode]
  (if (= mode 0)
    (get mem (get mem idx) 0)
    (get mem idx 0)))

(defn run-instruction [mem idx]
  (let [opcode (mod (get mem idx) 100)
        mode1 (quot (mod (get mem idx) 1000) 100)
        mode2 (quot (mod (get mem idx) 10000) 1000)
        param1 (get-param mem (+ idx 1) mode1)
        param2 (get-param mem (+ idx 2) mode2)
        result-idx (get mem (+ idx 3))]
    (cond
      (= opcode 1) (recur (assoc mem result-idx (+ param1 param2)) (+ idx 4))
      (= opcode 2) (recur (assoc mem result-idx (* param1 param2)) (+ idx 4))
      (= opcode 3) (recur (assoc mem (get mem (+ idx 1)) 1) (+ idx 2))
      (= opcode 4) (do (println param1) (recur mem (+ idx 2)))
      (= opcode 99) mem)))

(defn -main []
  (let [input (slurp "input.txt")
        memory (vec (map read-string (clojure.string/split input #",")))]
    (run-instruction memory 0)))

(-main)
