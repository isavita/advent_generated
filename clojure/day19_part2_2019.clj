
(ns solution
  (:require [clojure.string :as str]))

(defn read-input []
  (->> (slurp "input.txt")
       str/trim
       (#(str/split % #","))
       (mapv #(Long/parseLong %))))

(defn create-memory [program]
  (into {} (map-indexed vector program)))

(defn get-memory [memory addr]
  (get memory addr 0))

(defn set-memory [memory addr val]
  (assoc memory addr val))

(defn run-intcode [program input]
  (loop [memory (create-memory program)
         i 0
         relative-base 0
         input-idx 0
         output 0]
    (let [opcode (mod (get-memory memory i) 100)
          mode1 (mod (quot (get-memory memory i) 100) 10)
          mode2 (mod (quot (get-memory memory i) 1000) 10)
          mode3 (mod (quot (get-memory memory i) 10000) 10)
          get-addr (fn [offset mode]
                     (let [param (get-memory memory (+ i offset))]
                       (case mode
                         0 param
                         1 (+ i offset)
                         2 (+ relative-base param))))
          get-val (fn [offset mode]
                    (get-memory memory (get-addr offset mode)))
          set-val (fn [offset val mode]
                    (set-memory memory (get-addr offset mode) val))]
      (case opcode
        1 (recur (set-val 3 (+ (get-val 1 mode1) (get-val 2 mode2)) mode3)
                 (+ i 4) relative-base input-idx output)
        2 (recur (set-val 3 (* (get-val 1 mode1) (get-val 2 mode2)) mode3)
                 (+ i 4) relative-base input-idx output)
        3 (recur (set-val 1 (nth input input-idx) mode1)
                 (+ i 2) relative-base (inc input-idx) output)
        4 (recur memory (+ i 2) relative-base input-idx (get-val 1 mode1))
        5 (recur memory
                 (if (not= 0 (get-val 1 mode1))
                   (get-val 2 mode2)
                   (+ i 3))
                 relative-base input-idx output)
        6 (recur memory
                 (if (= 0 (get-val 1 mode1))
                   (get-val 2 mode2)
                   (+ i 3))
                 relative-base input-idx output)
        7 (recur (set-val 3 (if (< (get-val 1 mode1) (get-val 2 mode2)) 1 0) mode3)
                 (+ i 4) relative-base input-idx output)
        8 (recur (set-val 3 (if (= (get-val 1 mode1) (get-val 2 mode2)) 1 0) mode3)
                 (+ i 4) relative-base input-idx output)
        9 (recur memory (+ i 2) (+ relative-base (get-val 1 mode1)) input-idx output)
        99 output
        (throw (ex-info "Invalid opcode" {:opcode opcode}))))))

(defn check-beam [program x y]
  (run-intcode program [x y]))

(defn part1 [program]
  (reduce + (for [y (range 50)
                  x (range 50)]
              (check-beam program x y))))

(defn part2 [program]
  (loop [x 0, y 99]
    (if (zero? (check-beam program x y))
      (recur (inc x) y)
      (if (= 1 (check-beam program (+ x 99) (- y 99)))
        (+ (* x 10000) (- y 99))
        (recur x (inc y))))))

(let [program (read-input)]
  (println (part1 program))
  (println (part2 program)))
