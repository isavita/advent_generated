
(ns day02.core
(:require [clojure.string :as str]
          [clojure.java.io :as io]))

(defn read-input []
  (with-open [reader (io/reader "input.txt")]
    (doall (line-seq reader))))

(defn process-line [line]
  (let [dimensions (str/split line #"x")]
    (if (not= 3 (count dimensions))
      (throw (IllegalArgumentException. "Invalid input format"))
      (let [l (Integer/parseInt (nth dimensions 0))
            w (Integer/parseInt (nth dimensions 1))
            h (Integer/parseInt (nth dimensions 2))]
        (let [bow (* l w h)
              sides (sort [l w h])
              wrap (+ (* 2 (nth sides 0)) (* 2 (nth sides 1)))]
          (+ bow wrap))))))

(defn part-one []
  (->> (read-input)
       (map process-line)
       (reduce +)))

(println (str (part-one)))
