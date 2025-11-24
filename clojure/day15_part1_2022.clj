
(ns beacon.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[sx sy bx by] (map #(Long/parseLong %) (re-seq #"-?\d+" line))]
    [[sx sy] [bx by]]))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn impossible [sensors y]
  (let [blocked (atom #{})]
    (doseq [[sensor beacon] sensors]
      (let [d (- (manhattan sensor beacon) (Math/abs (- (second sensor) y)))]
        (when (>= d 0)
          (let [x0 (first sensor)]
            (doseq [dx (range (- x0 d) (inc (+ x0 d)))]
              (swap! blocked conj dx))))))
    (doseq [[_ beacon] sensors]
      (when (= (second beacon) y)
        (swap! blocked disj (first beacon))))
    (count @blocked)))

(defn -main []
  (let [lines (str/split-lines (slurp "input.txt"))
        sensors (mapv parse-line lines)]
    (println (impossible sensors 2000000))))

(-main)
