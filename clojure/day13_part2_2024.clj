
(ns clojure-solution
  (:require [clojure.string :as str]))

(def ^:const offset 10000000000000)

(defn parse-coords [line]
  (let [nums (re-seq #"[+-]?\d+" line)]
    (mapv parse-long nums)))

(defn solve-machine [ax ay bx by px py]
  (let [px (+ px offset)
        py (+ py offset)
        D  (- (* ax by) (* ay bx))
        numA (- (* px by) (* py bx))
        numB (- (* py ax) (* px ay))]
    (when (and (not (zero? D))
               (zero? (mod numA D))
               (zero? (mod numB D)))
      (let [a (/ numA D)
            b (/ numB D)]
        (when (and (>= a 0) (>= b 0))
          (+ (* 3 a) b))))))

(defn -main []
  (let [blocks (str/split (slurp "input.txt") #"\n\s*\n")
        costs  (keep (fn [block]
                       (let [[a b p] (str/split-lines block)
                             [ax ay] (parse-coords a)
                             [bx by] (parse-coords b)
                             [px py] (parse-coords p)]
                         (solve-machine ax ay bx by px py)))
                     blocks)]
    (println (count costs) (reduce + costs))))

(-main)
