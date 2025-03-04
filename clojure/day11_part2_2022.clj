
(ns day11
  (:require [clojure.string :as str]))

(defn parse-monkey [monkey-str]
  (let [[_ items operation test true-target false-target] (str/split-lines monkey-str)
        starting-items (mapv #(Long/parseLong %) (re-seq #"\d+" items))
        operation-fn (let [[_ op arg] (re-find #"new = old (.) (.*)" operation)]
                       (case op
                         "*" (if (= arg "old") #(* % %) #( * % (Long/parseLong arg)))
                         "+" #(+ % (Long/parseLong arg))))
        test-divisor (Long/parseLong (re-find #"\d+" test))
        true-target (Long/parseLong (re-find #"\d+" true-target))
        false-target (Long/parseLong (re-find #"\d+" false-target))]
    {:items        starting-items
     :operation    operation-fn
     :test         test-divisor
     :true-target  true-target
     :false-target false-target
     :inspections  0}))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (mapv parse-monkey)))

(defn monkey-turn [monkeys monkey-idx divisor-part1? common-multiple]
  (let [monkey (get monkeys monkey-idx)
        {:keys [items operation test true-target false-target inspections] :as m} monkey]
    (reduce (fn [ms item]
                (let [new-worry (if divisor-part1?
                                  (quot (operation item) 3)
                                  (mod (operation item) common-multiple)
                                 )
                      target (if (zero? (mod new-worry test)) true-target false-target)]
                    (-> ms
                        (update-in [target :items] conj new-worry)
                        (update-in [monkey-idx :inspections] inc))))
              monkeys items)))

(defn simulate-rounds [monkeys num-rounds divisor-part1?]
   (let [common-multiple (reduce * (map :test monkeys))]
    (loop [ms monkeys
           round 0]
      (if (= round num-rounds)
        ms
        (let [updated-monkeys (reduce (fn [acc monkey-idx]
                                         (-> (monkey-turn acc monkey-idx divisor-part1? common-multiple)
                                             (update-in [monkey-idx :items] (constantly []))))
                                       ms (range (count ms)))]
          (recur updated-monkeys (inc round)))))))

(defn monkey-business [monkeys]
  (->> monkeys
       (map :inspections)
       (sort >)
       (take 2)
       (reduce *)))

(defn part1 [monkeys]
  (monkey-business (simulate-rounds monkeys 20 true)))

(defn part2 [monkeys]
  (monkey-business (simulate-rounds monkeys 10000 false)))

(defn -main []
  (let [input (slurp "input.txt")
        monkeys (parse-input input)]
    (println "Part 1:" (part1 monkeys))
    (println "Part 2:" (part2 monkeys))))

(-main)
