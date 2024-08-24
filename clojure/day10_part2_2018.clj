(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map (fn [line]
                   (let [[_ x y vx vy] (re-matches #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"
                                                 line)]
                     {:x (Integer/parseInt x)
                      :y (Integer/parseInt y)
                      :vx (Integer/parseInt vx)
                      :vy (Integer/parseInt vy)}))
                 (line-seq rdr)))))

(defn move-points [points]
  (map (fn [p]
         (assoc p :x (+ (:x p) (:vx p))
                   :y (+ (:y p) (:vy p))))
       points))

(defn bounding-box [points]
  (let [xs (map :x points)
        ys (map :y points)]
    {:min-x (apply min xs)
     :max-x (apply max xs)
     :min-y (apply min ys)
     :max-y (apply max ys)}))

(defn area [bbox]
  (* (- (:max-x bbox) (:min-x bbox))
     (- (:max-y bbox) (:min-y bbox))))

(defn print-points [points]
  (let [bbox (bounding-box points)
        grid (vec (repeat (+ 1 (- (:max-y bbox) (:min-y bbox)))
                          (vec (repeat (+ 1 (- (:max-x bbox) (:min-x bbox))) \.))))]
    (doseq [p points]
      (let [x (- (:x p) (:min-x bbox))
            y (- (:y p) (:min-y bbox))]
        (assoc-in grid [y x] \#)))
    (doseq [row grid]
      (println (apply str row)))))

(defn -main []
  (let [points (parse-input "input.txt")]
    (loop [points points
           min-area (area (bounding-box points))
           min-points points
           seconds 0]
      (let [moved-points (move-points points)
            current-area (area (bounding-box moved-points))]
        (if (< current-area min-area)
          (recur moved-points current-area moved-points (inc seconds))
          (do
            (println "Message appears after" seconds "seconds:")
            (print-points min-points)))))))

(-main)