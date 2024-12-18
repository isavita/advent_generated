
(ns claw-contraption
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ ax ay bx by px py] (re-matches #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)" line)]
    {:a {:x (parse-long ax) :y (parse-long ay)}
     :b {:x (parse-long bx) :y (parse-long by)}
     :prize {:x (parse-long px) :y (parse-long py)}}))

(defn solve-machine [machine]
  (let [a (:a machine)
        b (:b machine)
        prize (:prize machine)]
    (reduce
     (fn [min-cost [na nb]]
       (let [x (+ (* na (:x a)) (* nb (:x b)))
             y (+ (* na (:y a)) (* nb (:y b)))]
         (if (and (= x (:x prize)) (= y (:y prize)))
           (let [cost (+ (* na 3) nb)]
             (if (or (nil? min-cost) (< cost min-cost))
               cost
               min-cost))
           min-cost)))
     nil
     (for [na (range 101) nb (range 101)] [na nb]))))

(defn solve [input]
  (let [machines (map parse-line (str/split input #"\n\n"))
        costs (map solve-machine machines)
        winnable-costs (filter some? costs)]
    (if (empty? winnable-costs)
      0
      (reduce + winnable-costs))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
