(ns fft
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (mapv #(Integer/parseInt (str %)) input))

(defn fft [input]
  (let [base-pattern [0 1 0 -1]
        output (vec (repeat (count input) 0))]
    (for [i (range (count input))]
      (mod (Math/abs (reduce + (map * input (for [j (range (count input))] (nth base-pattern (mod (quot (inc j) (inc i)) 4)))))) 10))))

(defn run-fft [input phases]
  (loop [current-input input phase 0]
    (if (= phase phases)
      current-input
      (recur (fft current-input) (inc phase)))))

(defn -main []
  (let [input (slurp "input.txt")
        parsed-input (parse-input input)]
    (println (apply str (take 8 (run-fft parsed-input 100))))))

(-main)