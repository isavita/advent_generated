
(ns intcode-computer
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (let [content (slurp filename)]
    (mapv #(Integer/parseInt %) (str/split content #","))))

(defn run-intcode [program]
  (loop [prog (vec program)
         pointer 0]
    (let [opcode (get prog pointer)]
      (cond
        (= opcode 99) prog
        (= opcode 1)
        (let [param1 (get prog (get prog (inc pointer)))
              param2 (get prog (get prog (+ pointer 2)))
              output-pos (get prog (+ pointer 3))]
          (recur (assoc prog output-pos (+ param1 param2)) (+ pointer 4)))
        (= opcode 2)
        (let [param1 (get prog (get prog (inc pointer)))
              param2 (get prog (get prog (+ pointer 2)))
              output-pos (get prog (+ pointer 3))]
          (recur (assoc prog output-pos (* param1 param2)) (+ pointer 4)))
        :else (throw (Exception. "Unknown opcode"))))))

(defn restore-program [program]
  (-> program
      (assoc 1 12)
      (assoc 2 2)))

(defn main []
  (let [program (parse-input "input.txt")
        restored-program (restore-program program)
        final-state (run-intcode restored-program)]
    (println "Value at position 0:" (get final-state 0))))

(main)
