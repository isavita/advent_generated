
(ns intcode.core
  (:require [clojure.string :as str]))

(defn parse-input []
  (->> (slurp "input.txt")
       str/trim
       (#(str/split % #","))
       (mapv #(Long/parseLong %))
       (map-indexed vector)
       (into {})))

(defn get-param [memory ip offset modes relative-base]
  (let [mode (if (>= (count modes) offset)
               (Long/parseLong (str (nth modes (- (count modes) offset))))
               0)
        param (get memory (+ ip offset) 0)]
    (case mode
      0 (get memory param 0)
      1 param
      2 (get memory (+ relative-base param) 0)
      (throw (Exception. "unknown parameter mode")))))

(defn set-param [memory ip offset modes relative-base value]
  (let [mode (if (>= (count modes) offset)
               (Long/parseLong (str (nth modes (- (count modes) offset))))
               0)
        param (get memory (+ ip offset) 0)]
    (case mode
      0 (assoc memory param value)
      2 (assoc memory (+ relative-base param) value)
      (throw (Exception. "unknown parameter mode")))))

(defn run-intcode [memory]
  (loop [memory memory
         ip 0
         relative-base 0
         output 0]
    (let [opcode (mod (get memory ip 0) 100)
          modes (str (quot (get memory ip 0) 100))]
      (case opcode
        1 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)
                new-memory (set-param memory ip 3 modes relative-base (+ arg1 arg2))]
            (recur new-memory (+ ip 4) relative-base output))
        2 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)
                new-memory (set-param memory ip 3 modes relative-base (* arg1 arg2))]
            (recur new-memory (+ ip 4) relative-base output))
        3 (let [new-memory (set-param memory ip 1 modes relative-base 1)]
            (recur new-memory (+ ip 2) relative-base output))
        4 (let [new-output (get-param memory ip 1 modes relative-base)]
            (recur memory (+ ip 2) relative-base new-output))
        5 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)]
            (if (not= arg1 0)
              (recur memory arg2 relative-base output)
              (recur memory (+ ip 3) relative-base output)))
        6 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)]
            (if (= arg1 0)
              (recur memory arg2 relative-base output)
              (recur memory (+ ip 3) relative-base output)))
        7 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)
                new-memory (set-param memory ip 3 modes relative-base (if (< arg1 arg2) 1 0))]
            (recur new-memory (+ ip 4) relative-base output))
        8 (let [arg1 (get-param memory ip 1 modes relative-base)
                arg2 (get-param memory ip 2 modes relative-base)
                new-memory (set-param memory ip 3 modes relative-base (if (= arg1 arg2) 1 0))]
            (recur new-memory (+ ip 4) relative-base output))
        9 (let [arg1 (get-param memory ip 1 modes relative-base)]
            (recur memory (+ ip 2) (+ relative-base arg1) output))
        99 output
        (throw (Exception. (str "unknown opcode: " opcode)))))))

(defn -main []
  (let [memory (parse-input)]
    (println (run-intcode memory))))

(-main)
