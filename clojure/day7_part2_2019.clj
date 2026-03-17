
(ns solution
  (:require [clojure.string :as str]))

(defn parse-program [input]
  (mapv #(Integer/parseInt %) (str/split (str/trim input) #",")))

(defn get-value [memory mode param]
  (if (= mode 1) param (get memory param 0)))

(defn run-computer [memory pc input output]
  (loop [memory memory pc pc input input output output]
    (let [instr (get memory pc)
          opcode (mod instr 100)
          modes (quot instr 100)]
      (case opcode
        1 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))
                dest (get memory (+ pc 3))]
            (recur (assoc memory dest (+ p1 p2)) (+ pc 4) input output))
        2 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))
                dest (get memory (+ pc 3))]
            (recur (assoc memory dest (* p1 p2)) (+ pc 4) input output))
        3 (let [dest (get memory (+ pc 1))]
            (recur (assoc memory dest (first input)) (+ pc 2) (rest input) output))
        4 (let [val (get-value memory (mod modes 10) (get memory (+ pc 1)))]
            {:memory memory :pc (+ pc 2) :input input :output val :halted false})
        5 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))]
            (if (not= p1 0)
              (recur memory p2 input output)
              (recur memory (+ pc 3) input output)))
        6 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))]
            (if (= p1 0)
              (recur memory p2 input output)
              (recur memory (+ pc 3) input output)))
        7 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))
                dest (get memory (+ pc 3))]
            (recur (assoc memory dest (if (< p1 p2) 1 0)) (+ pc 4) input output))
        8 (let [p1 (get-value memory (mod modes 10) (get memory (+ pc 1)))
                p2 (get-value memory (mod (quot modes 10) 10) (get memory (+ pc 2)))
                dest (get memory (+ pc 3))]
            (recur (assoc memory dest (if (= p1 p2) 1 0)) (+ pc 4) input output))
        99 {:memory memory :pc pc :input input :output output :halted true}))))

(defn run-amplifiers [program phase-settings]
  (let [initial-states (mapv (fn [phase] {:memory program :pc 0 :input [phase] :output 0 :halted false}) phase-settings)]
    (loop [states initial-states input-signal 0 index 0]
      (if (>= index (count states))
        (if (:halted (peek states))
          input-signal
          (recur states input-signal 0))
        (let [state (states index)
              new-state (run-computer (:memory state) (:pc state) (conj (:input state) input-signal) (:output state))]
          (if (:halted new-state)
            (if (= index (dec (count states)))
              (:output new-state)
              (recur (assoc states index new-state) input-signal (inc index)))
            (recur (assoc states index new-state) (:output new-state) (inc index))))))))

(defn permutations [coll]
  (if (empty? coll)
    '(())
    (for [x coll
          y (permutations (disj (set coll) x))]
      (cons x y))))

(defn solve []
  (let [program (parse-program (slurp "input.txt"))
        phase-settings [5 6 7 8 9]
        perms (permutations phase-settings)]
    (apply max (map #(run-amplifiers program %) perms))))

(println (solve))
