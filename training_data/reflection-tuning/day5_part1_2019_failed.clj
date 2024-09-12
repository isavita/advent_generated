(defn parse-opcode [instruction]
  (let [opcode (mod instruction 100)
        modes (->> (/ instruction 100)
                   (iterate #(quot % 10))
                   (take 3)
                   (mapv #(mod % 10)))]
    [opcode modes]))

(defn get-value [memory address mode]
  (if (zero? mode)
    (get memory (get memory address))
    (get memory address)))

(defn intcode [memory input]
  (loop [mem memory
         pc 0
         input input
         output []]
    (let [[opcode modes] (parse-opcode (get mem pc))
          params (subvec mem (inc pc) (+ pc 4))]
      (case opcode
        99 (last output)
        1 (recur (assoc mem (get params 2) (+ (get-value mem (get params 0) (get modes 0))
                                              (get-value mem (get params 1) (get modes 1))))
                 (+ pc 4)
                 input
                 output)
        2 (recur (assoc mem (get params 2) (* (get-value mem (get params 0) (get modes 0))
                                              (get-value mem (get params 1) (get modes 1))))
                 (+ pc 4)
                 input
                 output)
        3 (recur (assoc mem (get params 0) (first input))
                 (+ pc 2)
                 (rest input)
                 output)
        4 (recur mem
                 (+ pc 2)
                 input
                 (conj output (get-value mem (get params 0) (get modes 0))))
        (throw (Exception. (str "Unknown opcode: " opcode)))))))

(defn solve [input]
  (let [program (vec (map read-string (clojure.string/split input #",")))]
    (intcode program [1])))

;; Example usage:
;; (solve "3,0,4,0,99")  ; Should output 1
;; (solve "1002,4,3,4,33")  ; Should modify memory and halt
;; Replace with actual puzzle input to get the diagnostic code
