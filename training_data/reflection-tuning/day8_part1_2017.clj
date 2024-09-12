(require '[clojure.string :as str])

(defn parse-instruction [line]
  (let [[reg op val _ cond-reg cond-op cond-val] (str/split line #"\s+")]
    {:reg reg
     :op ({"inc" + "dec" -} op)
     :val (read-string val)
     :cond-reg cond-reg
     :cond-op cond-op
     :cond-val (read-string cond-val)}))

(defn evaluate-condition [registers cond-reg cond-op cond-val]
  (let [reg-val (get registers cond-reg 0)
        op ({">" > "<" < ">=" >= "<=" <= "==" = "!=" not=}
            cond-op)]
    (op reg-val cond-val)))

(defn execute-instruction [registers {:keys [reg op val cond-reg cond-op cond-val]}]
  (if (evaluate-condition registers cond-reg cond-op cond-val)
    (update registers reg (fnil op 0) val)
    registers))

(defn process-instructions [instructions]
  (reduce execute-instruction {} instructions))

(defn solve-puzzle []
  (let [instructions (->> (slurp "input.txt")
                          str/split-lines
                          (map parse-instruction))
        final-registers (process-instructions instructions)]
    (apply max (vals final-registers))))

(println (solve-puzzle))
