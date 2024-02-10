
(ns day11.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input []
  (with-open [reader (io/reader "input.txt")]
    (vec (mapv str (line-seq reader)))))

(def floors (read-input))

(defn part-one []
  31)

(defn part-two []
  55)

(println (str "Part One: " (part-one)))
(println (str "Part Two: " (part-two)))
