
(ns day10.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn look-and-say [s]
  (apply str (mapcat #(vector (count %) (first %)) (partition-by identity s))))

(defn part-one []
  (with-open [reader (io/reader "input.txt")]
    (let [input (str (first (line-seq reader)))
          result (nth (iterate look-and-say input) 40)]
      (count result))))

(println (str (part-one)))
