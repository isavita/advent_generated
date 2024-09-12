(ns day8.core
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[op arg] (str/split line #" ")]
    [op (Integer/parseInt arg)]))

(defn parse-input [filename]
  (->> (slurp filename)
       str/split-lines
       (mapv parse-instruction)))

(defn execute-boot-code [instructions]
  (loop [pc 0
         acc 0
         visited #{}]
    (cond
      (visited pc) {:loop true :acc acc}
      (>= pc (count instructions)) {:loop false :acc acc}
      :else
      (let [[op arg] (instructions pc)]
        (case op
          "acc" (recur (inc pc) (+ acc arg) (conj visited pc))
          "jmp" (recur (+ pc arg) acc (conj visited pc))
          "nop" (recur (inc pc) acc (conj visited pc)))))))

(defn swap-instruction [instructions idx]
  (update-in instructions [idx 0] {"jmp" "nop" "nop" "jmp"}))

(defn fix-boot-code [instructions]
  (first (for [i (range (count instructions))
               :let [[op _] (instructions i)]
               :when (contains? #{"jmp" "nop"} op)
               :let [modified (swap-instruction instructions i)
                     result (execute-boot-code modified)]
               :when (not (:loop result))]
           (:acc result))))

(defn solve-part1 [instructions]
  (:acc (execute-boot-code instructions)))

(defn solve-part2 [instructions]
  (fix-boot-code instructions))

(defn -main []
  (let [instructions (parse-input "input.txt")]
    (println "Part 1:" (solve-part1 instructions))
    (println "Part 2:" (solve-part2 instructions))))

(-main)
