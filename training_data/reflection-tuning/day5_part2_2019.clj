(ns intcode-computer
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (mapv read-string (str/split (slurp filename) #",")))

(defn get-param [memory pos mode]
  (let [val (memory pos)]
    (if (zero? mode) (memory val) val)))

(defn run-program [memory input]
  (loop [mem (transient memory)
         ip 0
         in input
         out []]
    (let [instruction (mem ip)
          opcode (mod instruction 100)
          mode1 (mod (quot instruction 100) 10)
          mode2 (mod (quot instruction 1000) 10)
          mode3 (mod (quot instruction 10000) 10)]
      (case opcode
        99 (last out)
        (1 2) (let [p1 (get-param mem (inc ip) mode1)
                    p2 (get-param mem (+ ip 2) mode2)
                    dest (mem (+ ip 3))
                    result (if (= opcode 1) (+ p1 p2) (* p1 p2))]
                (recur (assoc! mem dest result) (+ ip 4) in out))
        3 (recur (assoc! mem (mem (inc ip)) (first in)) (+ ip 2) (rest in) out)
        4 (recur mem (+ ip 2) in (conj out (get-param mem (inc ip) mode1)))
        (5 6) (let [p1 (get-param mem (inc ip) mode1)
                    p2 (get-param mem (+ ip 2) mode2)]
                (if (= (zero? p1) (= opcode 6))
                  (recur mem p2 in out)
                  (recur mem (+ ip 3) in out)))
        (7 8) (let [p1 (get-param mem (inc ip) mode1)
                    p2 (get-param mem (+ ip 2) mode2)
                    dest (mem (+ ip 3))
                    result (if (= opcode 7)
                             (if (< p1 p2) 1 0)
                             (if (= p1 p2) 1 0))]
                (recur (assoc! mem dest result) (+ ip 4) in out))))))

(let [program (parse-input "input.txt")]
  (println "Part One:" (run-program program [1]))
  (println "Part Two:" (run-program program [5])))
