(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input []
  (let [s (slurp "input.txt")
        [players last-marble] (map parse-long (re-seq #"\d+" s))]
    [players (* 100 last-marble)]))

(defn solve [players last-marble]
  (let [n (+ (* 2 last-marble) 10)
        nexts (long-array n)
        prevs (long-array n)
        scores (long-array players)]
    (aset-long nexts 0 0)
    (aset-long prevs 0 0)
    (loop [marble 1
           current 0]
      (if (> marble last-marble)
        (apply max (map long scores))
        (if (zero? (mod marble 23))
          (let [player (mod marble players)
                c1 (aget prevs current)
                c2 (aget prevs c1)
                c3 (aget prevs c2)
                c4 (aget prevs c3)
                c5 (aget prevs c4)
                c6 (aget prevs c5)
                c7 (aget prevs c6)
                removed c7
                before (aget prevs removed)
                after (aget nexts removed)
                score (+ marble removed)]
            (aset-long scores player (+ (aget scores player) score))
            (aset-long nexts before after)
            (aset-long prevs after before)
            (recur (inc marble) after))
          (let [a (aget nexts current)
                b (aget nexts a)]
            (aset-long nexts a marble)
            (aset-long prevs marble a)
            (aset-long nexts marble b)
            (aset-long prevs b marble)
            (recur (inc marble) marble)))))))

(defn -main []
  (let [[players last-marble] (parse-input)]
    (println (solve players last-marble))))

(-main)