
(ns day18
  (:require [clojure.string :as str]))

(defn evaluate-simple [expr]
  (let [nums (map #(Long/parseLong %) (re-seq #"\d+" expr))
        ops (re-seq #"[\+\*]" expr)]
    (reduce (fn [acc [op n]]
              (if (= op "+") (+ acc n) (* acc n)))
            (first nums)
            (map vector ops (rest nums)))))

(defn evaluate [expr]
  (loop [e expr]
    (if (str/includes? e "(")
      (recur (str/replace e #"\([^()]*\)"
                          (fn [m]
                            (str (evaluate-simple (subs m 1 (dec (count m))))))))
      (evaluate-simple e))))

(defn evaluate-advanced-simple [expr]
  (loop [e expr]
    (if (str/includes? e "+")
      (recur (str/replace e #"\d+ \+ \d+"
                          (fn [m]
                            (str (reduce + (map #(Long/parseLong %) (str/split m #" \+ ")))))))
      (reduce * (map #(Long/parseLong %) (str/split e #" \* "))))))

(defn evaluate-advanced [expr]
  (loop [e expr]
    (if (str/includes? e "(")
      (recur (str/replace e #"\([^()]*\)"
                          (fn [m]
                            (str (evaluate-advanced-simple (subs m 1 (dec (count m))))))))
      (evaluate-advanced-simple e))))

(defn -main [& _]
  (let [lines (str/split-lines (slurp "input.txt"))]
    (println (reduce + (map evaluate lines)))
    (println (reduce + (map evaluate-advanced lines)))))

(-main)
