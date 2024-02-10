
(ns day02.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn valid-password? [line]
  (let [[_ min max char password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    (let [count (count (re-seq (re-pattern char) password))]
      (and (<= (Integer/parseInt min) count (Integer/parseInt max))))))

(defn part-one []
  (with-open [xin (io/reader "input.txt")]
    (count (filter valid-password? (line-seq xin)))))

(println (str (part-one)))
