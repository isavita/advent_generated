(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn max [x y]
  (if (> x y) x y))

(defn distance [x y z]
  (/ (+ (abs x) (abs y) (abs z)) 2))

(defn -main []
  (with-open [r (io/reader "input.txt")]
    (let [input (str/trim (first (line-seq r)))
          directions (str/split input #",")]
      (loop [x 0 y 0 z 0 max-distance 0 dirs directions]
        (if (empty? dirs)
          (println max-distance)
          (let [dir (first dirs)
                [x y z] (cond
                          (= dir "n") [x (inc y) (dec z)]
                          (= dir "ne") [(inc x) y (dec z)]
                          (= dir "se") [(inc x) (dec y) z]
                          (= dir "s") [x (dec y) (inc z)]
                          (= dir "sw") [(dec x) y (inc z)]
                          (= dir "nw") [(dec x) (inc y) z])]
            (recur x y z (max max-distance (distance x y z)) (rest dirs))))))))

(-main)