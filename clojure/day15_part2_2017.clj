(ns solution
  (:require [clojure.java.io :as io]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (line-seq rdr)]
      [(Integer/parseInt (nth lines 0))
       (Integer/parseInt (nth lines 1))])))

(defn solve []
  (let [[genA-start genB-start] (read-input)
        genA-factor 16807
        genB-factor 48271
        modulus 2147483647
        genA-multiple 4
        genB-multiple 8
        genA (atom genA-start)
        genB (atom genB-start)
        matches (atom 0)]
    
    (dotimes [i 5000000]
      (loop []
        (reset! genA (mod (* @genA genA-factor) modulus))
        (when (not= 0 (mod @genA genA-multiple))
          (recur)))
      (loop []
        (reset! genB (mod (* @genB genB-factor) modulus))
        (when (not= 0 (mod @genB genB-multiple))
          (recur)))
      (when (= (bit-and @genA 0xFFFF) (bit-and @genB 0xFFFF))
        (swap! matches inc)))
    (println @matches)))

(solve)