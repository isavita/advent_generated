
(defn check-sequence [scoreboard sequence]
  (let [len-scoreboard (count scoreboard)
        len-sequence (count sequence)]
    (if (< len-scoreboard len-sequence)
      false
      (let [start (- len-scoreboard len-sequence)]
        (= (subvec scoreboard start) sequence)))))

(let [input (slurp "input.txt")
      input-sequence (map #(Character/digit % 10) input)
      input-len (count input-sequence)]
  (loop [scoreboard [3 7]
         elf1 0
         elf2 1]
    (let [new-score (+ (nth scoreboard elf1) (nth scoreboard elf2))
          scoreboard (if (>= new-score 10)
                       (conj scoreboard (quot new-score 10))
                       scoreboard)
          scoreboard (conj scoreboard (rem new-score 10))]
      (if (or (check-sequence scoreboard input-sequence)
              (and (>= new-score 10) (check-sequence (pop scoreboard) input-sequence)))
        (println (- (count scoreboard) input-len (if (and (>= new-score 10) (check-sequence (pop scoreboard) input-sequence)) 1 0)))
        (let [len-scoreboard (count scoreboard)
              elf1 (mod (+ elf1 (nth scoreboard elf1) 1) len-scoreboard)
              elf2 (mod (+ elf2 (nth scoreboard elf2) 1) len-scoreboard)]
          (recur scoreboard elf1 elf2))))))
