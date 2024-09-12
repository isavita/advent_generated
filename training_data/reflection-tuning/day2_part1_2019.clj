(ns intcode-computer
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (mapv read-string (str/split (slurp filename) #",")))

(defn execute-opcode [memory pos]
  (let [opcode (memory pos)
        a (memory (memory (+ pos 1)))
        b (memory (memory (+ pos 2)))
        dest (memory (+ pos 3))]
    (case opcode
      1 (assoc memory dest (+ a b))
      2 (assoc memory dest (* a b))
      99 nil
      (throw (Exception. (str "Unknown opcode: " opcode))))))

(defn run-program [memory]
  (loop [mem memory
         pos 0]
    (if-let [new-mem (execute-opcode mem pos)]
      (recur new-mem (+ pos 4))
      mem)))

(defn solve []
  (let [initial-memory (parse-input "input.txt")
        modified-memory (-> initial-memory
                            (assoc 1 12)
                            (assoc 2 2))
        final-memory (run-program modified-memory)]
    (println (first final-memory))))

(solve)
