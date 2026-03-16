
(ns springscript.core
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (->> (slurp filename)
       str/trim
       (#(str/split % #","))
       (mapv #(Long/parseLong %))))

(defn get-mode [cmd param-idx]
  (-> (quot cmd 100)
      (quot (long (Math/pow 10 param-idx)))
      (mod 10)))

(defn resolve-param [memory ip param-idx relative-base]
  (let [pos (+ ip param-idx 1)
        val (get memory pos 0)
        mode (get-mode (get memory ip) param-idx)]
    (case mode
      0 (get memory val 0)     ; position mode
      1 val                    ; immediate mode
      2 (get memory (+ relative-base val) 0)))) ; relative mode

(defn resolve-address [memory ip param-idx relative-base]
  (let [pos (+ ip param-idx 1)
        val (get memory pos 0)
        mode (get-mode (get memory ip) param-idx)]
    (case mode
      0 val                    ; position mode
      1 pos                    ; immediate mode (for writes)
      2 (+ relative-base val)))) ; relative mode

(defn run-vm [memory input]
  (loop [memory memory, ip 0, relative-base 0, input input, output []]
    (let [cmd (get memory ip)
          opcode (mod cmd 100)]
      (case opcode
        99 {:memory memory, :output output} ; halt
        
        1 ; add
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)
              addr (resolve-address memory ip 2 relative-base)]
          (recur (assoc memory addr (+ p1 p2)) (+ ip 4) relative-base input output))
        
        2 ; multiply
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)
              addr (resolve-address memory ip 2 relative-base)]
          (recur (assoc memory addr (* p1 p2)) (+ ip 4) relative-base input output))
        
        3 ; input
        (let [addr (resolve-address memory ip 0 relative-base)]
          (if (empty? input)
            {:memory memory, :output output, :ip ip, :relative-base relative-base, :waiting true}
            (recur (assoc memory addr (first input)) (+ ip 2) relative-base (rest input) output)))
        
        4 ; output
        (let [val (resolve-param memory ip 0 relative-base)]
          (recur memory (+ ip 2) relative-base input (conj output val)))
        
        5 ; jump-if-true
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)]
          (if (not= p1 0)
            (recur memory p2 relative-base input output)
            (recur memory (+ ip 3) relative-base input output)))
        
        6 ; jump-if-false
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)]
          (if (= p1 0)
            (recur memory p2 relative-base input output)
            (recur memory (+ ip 3) relative-base input output)))
        
        7 ; less than
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)
              addr (resolve-address memory ip 2 relative-base)]
          (recur (assoc memory addr (if (< p1 p2) 1 0)) (+ ip 4) relative-base input output))
        
        8 ; equals
        (let [p1 (resolve-param memory ip 0 relative-base)
              p2 (resolve-param memory ip 1 relative-base)
              addr (resolve-address memory ip 2 relative-base)]
          (recur (assoc memory addr (if (= p1 p2) 1 0)) (+ ip 4) relative-base input output))
        
        9 ; adjust relative base
        (let [p1 (resolve-param memory ip 0 relative-base)]
          (recur memory (+ ip 2) (+ relative-base p1) input output))))))

(defn string->ascii [s]
  (concat (map int s) [10]))

(defn solve []
  (let [memory (into {} (map-indexed vector (parse-input "input.txt")))
        instructions ["NOT A J" "NOT B T" "OR T J" "NOT C T" "OR T J" "AND D J" "WALK"]
        input (mapcat string->ascii instructions)
        result (run-vm memory input)]
    (println (last (:output result)))))

(solve)
