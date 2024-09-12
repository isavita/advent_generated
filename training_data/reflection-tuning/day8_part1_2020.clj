(require '[clojure.string :as str])

(defn parse-instruction [line]
  (let [[op arg] (str/split line #" ")]
    [op (Integer/parseInt arg)]))

(defn execute [instructions]
  (loop [pc 0
         acc 0
         visited #{}]
    (if (contains? visited pc)
      acc
      (let [[op arg] (get instructions pc)]
        (case op
          "acc" (recur (inc pc) (+ acc arg) (conj visited pc))
          "jmp" (recur (+ pc arg) acc (conj visited pc))
          "nop" (recur (inc pc) acc (conj visited pc)))))))

(defn solve []
  (->> (slurp "input.txt")
       str/split-lines
       (mapv parse-instruction)
       execute
       println))

(solve)
