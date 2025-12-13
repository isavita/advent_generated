
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.math BigInteger]))

(defn add-big [m k ^BigInteger v]
  (assoc m k (.add (get m k BigInteger/ZERO) v)))

(defn step [counts width grid y]
  (reduce-kv
   (fn [acc x cnt]
     (let [split? (and (< -1 x width) (= \^ (nth (nth grid y) x)))]
       (if split?
         (-> acc (add-big (dec x) cnt) (add-big (inc x) cnt))
         (add-big acc x cnt))))
   {} counts))

(defn -main [& _]
  (let [grid   (str/split-lines (slurp "input.txt"))
        start  (some (fn [[y line]]
                       (when-let [x (str/index-of line \S)]
                         [x y]))
                     (map-indexed vector grid))]
    (when-not start
      (throw (Exception. "Start point 'S' not found")))
    (let [[start-x start-y] start
          width  (count (first grid))
          init   {start-x (BigInteger. "1")}
          final  (reduce (fn [cnts y] (step cnts width grid y))
                         init
                         (range (inc start-y) (count grid)))]
      (println (reduce #(.add %1 (val %2)) BigInteger/ZERO final)))))

(-main)
