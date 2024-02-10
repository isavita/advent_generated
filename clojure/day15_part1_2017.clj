
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input []
  (with-open [reader (io/reader "input.txt")]
    (let [lines (line-seq reader)
          genAStart (Long/parseLong (first lines))
          genBStart (Long/parseLong (second lines))]
      [genAStart genBStart])))

(defn solve []
  (let [[genAStart genBStart] (read-input)
        genAFactor 16807
        genBFactor 48271
        modulus 2147483647
        mask 65535]
    (loop [genA genAStart
           genB genBStart
           matches 0
           i 0]
      (if (= i 40000000)
        matches
        (let [genA (mod (* genA genAFactor) modulus)
              genB (mod (* genB genBFactor) modulus)]
          (recur genA genB
                 (if (= (bit-and genA mask) (bit-and genB mask))
                   (inc matches)
                   matches)
                 (inc i)))))))

(println (solve))
