
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn abs [n] (if (neg? n) (- n) n))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn read-sensors [file]
  (for [line (str/split-lines (slurp file))
        :let [[sx sy bx by] (map #(Long/parseLong %) (re-seq #"-?\d+" line))
              sensor [sx sy]
              beacon [bx by]
              dist (manhattan sensor beacon)]]
    {:pos sensor :beacon beacon :dist dist}))

(defn distress [sensors maxcoord]
  (let [maxcoord (long maxcoord)]
    (loop [x 0]
      (when (<= x maxcoord)
        (let [y (loop [y 0]
                  (when (<= y maxcoord)
                    (let [p [x y]
                          detected (volatile! false)
                          skip (volatile! 0)]
                      (doseq [{:keys [pos dist]} sensors]
                        (when (<= (manhattan pos p) dist)
                          (vreset! detected true)
                          (let [dist (- dist (abs (- (first pos) x)))
                                s (+ dist (second pos) (- y))]
                            (vswap! skip max s))))
                      (if @detected
                        (recur (inc (+ y @skip)))
                        y))))]
          (if y
            (+ (* x 4000000) y)
            (recur (inc x))))))))

(defn -main []
  (let [sensors (read-sensors "input.txt")]
    (println (distress sensors 4000000))))

(-main)
