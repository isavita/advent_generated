(ns solution
  (:require [clojure.string :as str]))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn manhattan-distance [x1 y1 x2 y2]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn parse-input [input]
  (map #(let [[x y] (str/split % #", ")]
          [(Integer/parseInt x) (Integer/parseInt y)])
       (str/split input #"\n")))

(defn main []
  (let [input (slurp "input.txt")
        coordinates (parse-input input)
        max-x (apply max (map first coordinates))
        max-y (apply max (map second coordinates))
        region-size (atom 0)]
    (doseq [x (range (inc max-x))
            y (range (inc max-y))]
      (let [total-distance (reduce + (map #(manhattan-distance x y (first %) (second %)) coordinates))]
        (when (< total-distance 10000)
          (swap! region-size inc))))
    (println @region-size)))

(main)