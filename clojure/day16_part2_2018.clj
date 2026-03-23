
(ns solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def ops
  {:addr #(assoc %1 %4 (+ (nth %1 %2) (nth %1 %3)))
   :addi #(assoc %1 %4 (+ (nth %1 %2) %3))
   :mulr #(assoc %1 %4 (* (nth %1 %2) (nth %1 %3)))
   :muli #(assoc %1 %4 (* (nth %1 %2) %3))
   :banr #(assoc %1 %4 (bit-and (nth %1 %2) (nth %1 %3)))
   :bani #(assoc %1 %4 (bit-and (nth %1 %2) %3))
   :borr #(assoc %1 %4 (bit-or (nth %1 %2) (nth %1 %3)))
   :bori #(assoc %1 %4 (bit-or (nth %1 %2) %3))
   :setr #(assoc %1 %4 (nth %1 %2))
   :seti #(assoc %1 %4 %2)
   :gtir #(assoc %1 %4 (if (> %2 (nth %1 %3)) 1 0))
   :gtri #(assoc %1 %4 (if (> (nth %1 %2) %3) 1 0))
   :gtrr #(assoc %1 %4 (if (> (nth %1 %2) (nth %1 %3)) 1 0))
   :eqir #(assoc %1 %4 (if (= %2 (nth %1 %3)) 1 0))
   :eqri #(assoc %1 %4 (if (= (nth %1 %2) %3) 1 0))
   :eqrr #(assoc %1 %4 (if (= (nth %1 %2) (nth %1 %3)) 1 0))})

(defn parse-ints [s] (mapv #(Long/parseLong %) (re-seq #"\d+" s)))

(defn solve []
  (let [input (slurp "input.txt")
        [samples-str program-str] (str/split (str/trim input) #"\n\n\n+")
        samples (->> (str/split samples-str #"\n\n")
                     (map (fn [s]
                            (let [lines (str/split-lines s)]
                              {:before (parse-ints (nth lines 0))
                               :instr (parse-ints (nth lines 1))
                               :after (parse-ints (nth lines 2))}))))
        program (->> (str/split-lines program-str)
                     (remove str/blank?)
                     (map parse-ints))
        
        match-ops (fn [sample]
                    (set (keys (filter (fn [[_ f]]
                                         (let [[_ a b c] (:instr sample)]
                                           (= (:after sample) (f (:before sample) a b c))))
                                       ops))))

        part1 (count (filter #(>= (count (match-ops %)) 3) samples))
        
        mapping (loop [poss (reduce (fn [acc s]
                                      (let [num (first (:instr s))]
                                        (update acc num set/intersection (match-ops s))))
                                    (into {} (for [i (range 16)] [i (set (keys ops))]))
                                    samples)
                       resolved {}]
                  (if (empty? poss)
                    resolved
                    (let [[num s] (first (filter #(= 1 (count (val %))) poss))
                          found (first s)]
                      (recur (-> (dissoc poss num)
                                 (update-vals #(disj % found)))
                             (assoc resolved num (ops found))))))
        
        part2 (first (reduce (fn [regs [op-num a b c]]
                               ((mapping op-num) regs a b c))
                             [0 0 0 0] program))]
    (println part1)
    (println part2)))

(solve)

