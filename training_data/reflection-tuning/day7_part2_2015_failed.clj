(ns day7
  (:require [clojure.string :as str]))

(defn parse-instruction [instruction]
  (let [[_ source dest] (re-matches #"(.+) -> (\w+)" instruction)]
    (if (and source dest)
      [dest (cond
              (re-matches #"\d+" source) (constantly (Integer/parseInt source))
              (re-matches #"\w+" source) #(% source)
              :else (let [[_ op arg1 arg2] (re-matches #"(\w+) (\w+)(?: (\w+))?" source)]
                      (case op
                        "AND" #(bit-and (% arg1) (% arg2))
                        "OR" #(bit-or (% arg1) (% arg2))
                        "LSHIFT" #(bit-shift-left (% arg1) (Integer/parseInt arg2))
                        "RSHIFT" #(bit-shift-right (% arg1) (Integer/parseInt arg2))
                        "NOT" #(bit-and 65535 (bit-not (% arg1))))))]
      (throw (Exception. (str "Invalid instruction: " instruction))))))

(defn build-circuit [instructions]
  (into {} (map parse-instruction instructions)))

(defn simulate-circuit [circuit]
  (let [memo (atom {})]
    (fn resolver [wire]
      (if-let [result (get @memo wire)]
        result
        (let [result (if (number? wire)
                       wire
                       ((get circuit wire) resolver))]
          (swap! memo assoc wire result)
          result)))))

(defn solve-part1 [input]
  (let [instructions (str/split-lines input)
        circuit (build-circuit instructions)
        simulator (simulate-circuit circuit)]
    (simulator "a")))

(defn solve-part2 [input]
  (let [instructions (str/split-lines input)
        circuit (build-circuit instructions)
        a-signal (solve-part1 input)
        new-circuit (assoc circuit "b" (constantly a-signal))
        simulator (simulate-circuit new-circuit)]
    (simulator "a")))

;; Example usage:
;; (def input (slurp "input.txt"))
;; (println "Part 1:" (solve-part1 input))
;; (println "Part 2:" (solve-part2 input))
