
(ns intcode.core
  (:require [clojure.string :as str]))

(defn parse-program [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(defn run-program [program noun verb]
  (let [memory (assoc (vec program) 1 noun 2 verb)]
    (loop [ip 0
           mem memory]
      (let [opcode (mem ip)]
        (case opcode
          1 (let [a (mem (mem (+ ip 1)))
                  b (mem (mem (+ ip 2)))
                  dest (mem (+ ip 3))]
              (recur (+ ip 4) (assoc mem dest (+ a b))))
          2 (let [a (mem (mem (+ ip 1)))
                  b (mem (mem (+ ip 2)))
                  dest (mem (+ ip 3))]
              (recur (+ ip 4) (assoc mem dest (* a b))))
          99 (first mem)
          (throw (Exception. (str "Unknown opcode: " opcode))))))))

(defn solve-part1 [program]
  (run-program program 12 2))

(defn solve-part2 [program target-output]
  (first
    (for [noun (range 100)
          verb (range 100)
          :let [result (try (run-program program noun verb)
                         (catch Exception _ nil))]
          :when (= result target-output)]
      (+ (* 100 noun) verb))))

(defn -main []
  (let [input (slurp "input.txt")
        program (parse-program input)]
    
    ;; Part 1
    (println (solve-part1 program))
    
    ;; Part 2
    (println (solve-part2 program 19690720))))

(-main)
