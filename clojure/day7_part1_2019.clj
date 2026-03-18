
(ns day7.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-program [s]
  (mapv #(Long/parseLong %) (str/split (str/trim s) #",")))

(defn run-program [program inputs]
  (loop [mem (vec program)
         ip 0
         inputs (seq inputs)
         outputs []]
    (let [cmd (nth mem ip)
          opcode (mod cmd 100)
          mode1 (mod (quot cmd 100) 10)
          mode2 (mod (quot cmd 1000) 10)
          mode3 (mod (quot cmd 10000) 10)]
      (case opcode
        1 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))
                dest (nth mem (+ 3 ip))]
            (recur (assoc mem dest (+ p1 p2)) (+ ip 4) inputs outputs))
        2 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))
                dest (nth mem (+ 3 ip))]
            (recur (assoc mem dest (* p1 p2)) (+ ip 4) inputs outputs))
        3 (let [dest (nth mem (inc ip))]
            (recur (assoc mem dest (first inputs)) (+ ip 2) (rest inputs) outputs))
        4 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))]
            (recur mem (+ ip 2) inputs (conj outputs p1)))
        5 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))]
            (if (not= p1 0)
              (recur mem p2 inputs outputs)
              (recur mem (+ ip 3) inputs outputs)))
        6 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))]
            (if (= p1 0)
              (recur mem p2 inputs outputs)
              (recur mem (+ ip 3) inputs outputs)))
        7 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))
                dest (nth mem (+ 3 ip))]
            (recur (assoc mem dest (if (< p1 p2) 1 0)) (+ ip 4) inputs outputs))
        8 (let [p1 (if (= mode1 1) (nth mem (inc ip)) (nth mem (nth mem (inc ip))))
                p2 (if (= mode2 1) (nth mem (+ 2 ip)) (nth mem (nth mem (+ 2 ip))))
                dest (nth mem (+ 3 ip))]
            (recur (assoc mem dest (if (= p1 p2) 1 0)) (+ ip 4) inputs outputs))
        99 outputs))))

(defn permutations [coll]
  (if (= 1 (count coll))
    [coll]
    (mapcat (fn [x]
              (map #(cons x %)
                   (permutations (remove #{x} coll))))
            coll)))

(defn run-amplifiers [program phases]
  (reduce (fn [signal phase]
            (first (run-program program [phase signal])))
          0
          phases))

(defn -main []
  (let [program (parse-program (slurp "input.txt"))
        max-signal (apply max
                          (map (fn [phases]
                                 (run-amplifiers program phases))
                               (permutations [0 1 2 3 4])))]
    (println max-signal)))

(-main)
