(ns day8-registers
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[_ reg op val cond-reg cond-op cond-val]
        (re-matches #"(\w+) (inc|dec) (-?\d+) if (\w+) ([<>=!]+) (-?\d+)" line)]
    {:reg reg
     :op ({"inc" + "dec" -} op)
     :val (Integer/parseInt val)
     :cond-reg cond-reg
     :cond-op cond-op
     :cond-val (Integer/parseInt cond-val)}))

(defn evaluate-condition [registers cond-reg cond-op cond-val]
  (let [reg-val (get @registers cond-reg 0)
        op ({">" > ">=" >= "<" < "<=" <= "==" = "!=" not=}
            cond-op)]
    (op reg-val cond-val)))

(defn process-instruction [registers max-ever {:keys [reg op val cond-reg cond-op cond-val]}]
  (when (evaluate-condition registers cond-reg cond-op cond-val)
    (let [new-val (op (get @registers reg 0) val)]
      (swap! registers assoc reg new-val)
      (swap! max-ever max new-val))))

(defn solve-registers [input]
  (let [instructions (map parse-instruction (str/split-lines input))
        registers (atom {})
        max-ever (atom 0)]
    (doseq [instruction instructions]
      (process-instruction registers max-ever instruction))
    [(apply max (vals @registers)) @max-ever]))

(defn -main []
  (let [input (slurp "input.txt")
        [part1 part2] (solve-registers input)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))

(-main)
