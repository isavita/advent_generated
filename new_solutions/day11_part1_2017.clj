(ns hex-ed
  (:require [clojure.string :as str]))

(defn abs [n]
  (if (< n 0) (- n) n))

(defn max [a b]
  (if (> a b) a b))

(defn distance [x y z]
  (/ (+ (abs x) (abs y) (abs z)) 2))

(defn move [x y z dir]
  (case dir
    "n" [x (inc y) (dec z)]
    "ne" [(inc x) y (dec z)]
    "se" [(inc x) (dec y) z]
    "s" [x (dec y) (inc z)]
    "sw" [(dec x) y (inc z)]
    "nw" [(dec x) (inc y) z]))

(defn -main []
  (let [input (slurp "input.txt")
        directions (str/split (str/trim input) #",")
        [x y z] (reduce (fn [[x y z] dir]
                          (move x y z dir))
                        [0 0 0] directions)]
    (println (distance x y z))))

(-main)