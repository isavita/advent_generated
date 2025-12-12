
(ns largest-area
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (when-let [[_ x y] (re-matches #"^\s*(-?\d+)\s*,\s*(-?\d+)\s*$" line)]
    [(Long/parseLong x) (Long/parseLong y)]))

(defn largest-area [points]
  (reduce max
          (for [[[x1 y1] [x2 y2]] (for [p1 points p2 points] [p1 p2])]
            (* (inc (Math/abs (- x1 x2)))
               (inc (Math/abs (- y1 y2)))))))

(defn -main [& _]
  (let [points (keep parse-line (line-seq (io/reader "input.txt")))]
    (println (largest-area points))))

(-main)
