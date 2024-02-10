(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [filename]
  (with-open [reader (io/reader filename)]
    (let [total-elves (Integer/parseInt (str/trim (first (line-seq reader))))]
      total-elves)))

(defn find-winning-elf [total-elves]
  (let [highest-power-of-two (loop [n 1]
                              (if (<= (* n 2) total-elves)
                                (recur (* n 2))
                                n))]
    (+ (* (- total-elves highest-power-of-two) 2) 1)))

(let [total-elves (read-input "input.txt")
      winner (find-winning-elf total-elves)]
  (println winner))