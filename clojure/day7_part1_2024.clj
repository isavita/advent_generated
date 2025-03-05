
(ns bridge-repair
  (:require [clojure.string :as str]))

(defn evaluate
  "Evaluates an expression with given numbers and operators (left-to-right)."
  [nums ops]
  (reduce (fn [acc [op num]]
            (case op
              :+ (+ acc num)
              :* (* acc num)))
          (first nums)
          (map vector ops (rest nums))))

(defn solve-equation
  "Checks if an equation can be solved with + and * operators."
  [target nums]
  (let [num-ops (dec (count nums))]
    (if (zero? num-ops)
        (= target (first nums))
        (some (fn [ops]
                 (= target (evaluate nums ops)))
              (for [i (range (long (Math/pow 2 num-ops)))]  ; Iterate through all combinations of + and *
                (map #(if (bit-test i %) :* :+) (range num-ops)))))))


(defn process-line
  "Processes a single line of input and returns the target value if solvable, otherwise 0."
  [line]
  (let [[target-str nums-str] (str/split line #": ")
        target (parse-long target-str)
        nums (map parse-long (str/split nums-str #" "))]
    (if (solve-equation target nums)
      target
      0)))

(defn -main
  "Reads input from input.txt, processes each line, and prints the total calibration result."
  [& args]
  (let [lines (str/split-lines (slurp "input.txt"))
        total-calibration (reduce + (map process-line lines))]
    (println total-calibration)))

;; Example usage (ensures the program runs when executed):
(-main)

