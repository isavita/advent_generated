(ns intcode-solver
  (:require [clojure.string :as str]))

(defn parse-input [file]
  (mapv read-string (str/split (slurp file) #",")))

(defn execute [memory]
  (loop [mem memory
         ip 0]
    (let [opcode (mem ip)]
      (if (= opcode 99)
        mem
        (let [a (mem (mem (+ ip 1)))
              b (mem (mem (+ ip 2)))
              dest (mem (+ ip 3))
              result (case opcode
                        1 (+ a b)
                        2 (* a b))]
          (recur (assoc mem dest result) (+ ip 4)))))))

(defn run-with-inputs [memory noun verb]
  (-> memory
      (assoc 1 noun)
      (assoc 2 verb)
      execute
      first))

(defn find-inputs [memory target]
  (first (for [noun (range 100)
               verb (range 100)
               :when (= target (run-with-inputs memory noun verb))]
           [noun verb])))

(defn solve []
  (let [memory (parse-input "input.txt")]
    (println "Part 1:" (run-with-inputs memory 12 2))
    (let [[noun verb] (find-inputs memory 19690720)]
      (println "Part 2:" (+ (* 100 noun) verb)))))

(solve)
