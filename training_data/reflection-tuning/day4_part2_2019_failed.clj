(ns advent-of-code.day4
  (:require [clojure.string :as str]))

(defn digits [n]
  (map #(Character/digit % 10) (str n)))

(defn increasing? [digits]
  (apply <= digits))

(defn has-double? [digits]
  (some #(= 2 (count %)) (partition-by identity digits)))

(defn valid-password? [n]
  (let [ds (digits n)]
    (and (increasing? ds)
         (has-double? ds))))

(defn solve [start end]
  (count (filter valid-password? (range start (inc end)))))

(defn -main [& args]
  (let [start 134564
        end 585159]
    (println "Number of valid passwords:" (solve start end))))
