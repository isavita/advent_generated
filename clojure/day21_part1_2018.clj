
(defn addr [registers abc-values]
  (assoc registers (nth abc-values 2) (+ (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1)))))

(defn addi [registers abc-values]
  (assoc registers (nth abc-values 2) (+ (nth registers (nth abc-values 0)) (nth abc-values 1))))

(defn mulr [registers abc-values]
  (assoc registers (nth abc-values 2) (* (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1)))))

(defn muli [registers abc-values]
  (assoc registers (nth abc-values 2) (* (nth registers (nth abc-values 0)) (nth abc-values 1))))

(defn banr [registers abc-values]
  (assoc registers (nth abc-values 2) (bit-and (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1)))))

(defn bani [registers abc-values]
  (assoc registers (nth abc-values 2) (bit-and (nth registers (nth abc-values 0)) (nth abc-values 1))))

(defn borr [registers abc-values]
  (assoc registers (nth abc-values 2) (bit-or (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1)))))

(defn bori [registers abc-values]
  (assoc registers (nth abc-values 2) (bit-or (nth registers (nth abc-values 0)) (nth abc-values 1))))

(defn setr [registers abc-values]
  (assoc registers (nth abc-values 2) (nth registers (nth abc-values 0))))

(defn seti [registers abc-values]
  (assoc registers (nth abc-values 2) (nth abc-values 0)))

(defn gtir [registers abc-values]
  (assoc registers (nth abc-values 2) (if (> (nth abc-values 0) (nth registers (nth abc-values 1))) 1 0)))

(defn gtri [registers abc-values]
  (assoc registers (nth abc-values 2) (if (> (nth registers (nth abc-values 0)) (nth abc-values 1)) 1 0)))

(defn gtrr [registers abc-values]
  (assoc registers (nth abc-values 2) (if (> (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1))) 1 0)))

(defn eqir [registers abc-values]
  (assoc registers (nth abc-values 2) (if (= (nth abc-values 0) (nth registers (nth abc-values 1))) 1 0)))

(defn eqri [registers abc-values]
  (assoc registers (nth abc-values 2) (if (= (nth registers (nth abc-values 0)) (nth abc-values 1)) 1 0)))

(defn eqrr [registers abc-values]
  (assoc registers (nth abc-values 2) (if (= (nth registers (nth abc-values 0)) (nth registers (nth abc-values 1))) 1 0)))

(def opcode-names-to-funcs
  {"addr" addr, "addi" addi,
   "mulr" mulr, "muli" muli,
   "banr" banr, "bani" bani,
   "borr" borr, "bori" bori,
   "setr" setr, "seti" seti,
   "gtir" gtir, "gtri" gtri, "gtrr" gtrr,
   "eqir" eqir, "eqri" eqri, "eqrr" eqrr})

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        instruction-pointer (Integer/parseInt (second (re-find #"#ip (\d+)" (first lines))))
        instructions (map (fn [l]
                            (let [[_ name a b c] (re-find #"(\w+) (\d+) (\d+) (\d+)" l)]
                              {:name name :abc-values [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c)]}))
                          (rest lines))]
    {:instructions instructions :instruction-pointer instruction-pointer :registers [0 0 0 0 0 0]}))

(defn tick [opcode-computer]
  (let [ip (get opcode-computer :instruction-pointer)
        registers (get opcode-computer :registers)
        inst-index (nth registers ip)
        instructions (get opcode-computer :instructions)]
    (if (>= inst-index (count instructions))
      (do (println "Out of range instruction, terminating...")
          (assoc opcode-computer :done true))
      (let [inst (nth instructions inst-index)
            opcode-func (get opcode-names-to-funcs (get inst :name))
            new-registers (opcode-func registers (get inst :abc-values))]
        (-> opcode-computer
            (assoc :registers (update new-registers ip inc))
            (assoc :done (>= (nth new-registers ip) (count instructions))))))))

(defn solve [input]
  (loop [opcode-computer (parse-input input)]
    (if (or (get opcode-computer :done) (= 28 (nth (get opcode-computer :registers) (get opcode-computer :instruction-pointer))))
      (nth (get opcode-computer :registers) 5)
      (recur (tick opcode-computer)))))

(println (solve (slurp "input.txt")))
