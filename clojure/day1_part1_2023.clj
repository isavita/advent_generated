(ns day01.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn is-digit [^Character c]
  (let [zero (int \0)
        nine (int \9)]
    (<= zero (int c) nine)))

(defn only-digits
  "return only the 'digit' chars"
  [^String s]
  (filter is-digit (seq s)))

(defn calib-value [^String s digit-conversion-function]
  "return the calibration value for s"
  (Integer/parseInt
   (let [dgs (digit-conversion-function s)]
     (str (first dgs) (last dgs)))))

(def digit-map
  {"one" 1, "two" 2, "three" 3, "four" 4, "five" 5, "six" 6, "seven" 7, "eight" 8, "nine" 9})

(def reversed-digit-map
  {"xis" 6, "eerht" 3, "owt" 2, "neves" 7, "evif" 5, "thgie" 8, "eno" 1, "enin" 9, "ruof" 4})

(defn int-ify-first-substring [line digit-map]
  (let [pattern (re-pattern (str "\\d|" (str/join "|" (keys digit-map))))]
    (let [match (re-find pattern line)]
      (if (= (count match) 1)
        (Integer/parseInt match)
        (digit-map match)))))

(defn part-one []
  (with-open [xin (io/reader "input.txt")]
    (reduce + (map #(calib-value % only-digits) (line-seq xin)))))

(println (str (part-one)))

