
(ns day7
  (:require [clojure.string :as str]))

(defn parse-instruction [instruction]
  (let [[input output] (str/split instruction #" -> ")
        parts (str/split input #" ")]
    {:output output
     :input parts}))

(defn resolve-value [signals value]
  (if (re-matches #"\d+" value)
    (Integer/parseInt value)
    (get signals value)))

(defn process-instruction [signals instruction]
  (let [{:keys [input output]} instruction]
    (try
      (cond
        (= (count input) 1)
        (let [[value] input
              resolved-value (if (re-matches #"\d+" value)
                               (Integer/parseInt value)
                               (resolve-value signals value))]
          (if (nil? resolved-value)
            signals
            (assoc signals output resolved-value)))

        (= (count input) 2)
        (let [[op value] input
              resolved-value (resolve-value signals value)]
          (if (nil? resolved-value)
            signals
            (assoc signals output (bit-not resolved-value))))

        (= (count input) 3)
        (let [[left op right] input
              resolved-left (if (re-matches #"\d+" left)
                              (Integer/parseInt left)
                              (resolve-value signals left))
              resolved-right (if (re-matches #"\d+" right)
                               (Integer/parseInt right)
                               (resolve-value signals right))]

          (if (or (nil? resolved-left) (nil? resolved-right))
            signals
            (assoc signals output
                   (case op
                     "AND" (bit-and resolved-left resolved-right)
                     "OR" (bit-or resolved-left resolved-right)
                     "LSHIFT" (bit-shift-left resolved-left resolved-right)
                     "RSHIFT" (bit-shift-right resolved-left resolved-right))))))
      (catch Exception e
        (println "Error processing instruction: " instruction)
        signals))))

(defn solve [instructions initial-signals]
  (loop [signals initial-signals
         remaining-instructions instructions]
    (if (empty? remaining-instructions)
      signals
      (let [new-signals (reduce
                          (fn [acc instruction]
                            (if (contains? acc (:output instruction))
                              acc
                              (process-instruction acc instruction)))
                          signals
                          remaining-instructions)
            unresolved-instructions (filter
                                      (fn [instruction]
                                        (not (contains? new-signals (:output instruction))))
                                      remaining-instructions)]
        (if (= (count unresolved-instructions) (count remaining-instructions))
          (do
            (println "Stuck: " unresolved-instructions)
            new-signals)
          (recur new-signals unresolved-instructions))))))

(defn main []
  (let [instructions (->> "input.txt"
                          slurp
                          str/split-lines
                          (map parse-instruction)
                          vec)
        part1-signals (solve instructions {})
        part1-a (get part1-signals "a")

        part2-instructions instructions
        part2-initial-signals {"b" part1-a} ; Override wire b
        part2-signals (solve part2-instructions part2-initial-signals)
        part2-a (get part2-signals "a")]

    (println "Part 1: Signal on wire a:" part1-a)
    (println "Part 2: Signal on wire a:" part2-a)))

(main)
