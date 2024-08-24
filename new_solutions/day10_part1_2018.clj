(ns day10
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (map #(let [[_ x y vx vy] (re-matches #".*<\s*(-?\d+),\s*(-?\d+)>.*<\s*(-?\d+),\s*(-?\d+)>.*" %)]
           {:x (Integer/parseInt x) :y (Integer/parseInt y) :vx (Integer/parseInt vx) :vy (Integer/parseInt vy)})
       (str/split-lines input)))

(defn move-points [points]
  (map #(update % :x + (:vx %)) 
       (map #(update % :y + (:vy %)) points)))

(defn bounding-box [points]
  (let [min-x (apply min (map :x points))
        max-x (apply max (map :x points))
        min-y (apply min (map :y points))
        max-y (apply max (map :y points))]
    {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}))

(defn area [bbox]
  (* (- (:max-x bbox) (:min-x bbox)) (- (:max-y bbox) (:min-y bbox))))

(defn print-points [points]
  (let [bbox (bounding-box points)
        points-set (set (map (fn [p] [(:x p) (:y p)]) points))]
    (doseq [y (range (:min-y bbox) (inc (:max-y bbox)))]
      (doseq [x (range (:min-x bbox) (inc (:max-x bbox)))]
        (if (points-set [x y])
          (print "#")
          (print ".")))
      (println))))

(defn -main []
  (let [input (slurp "input.txt")
        points (parse-input input)]
    (loop [points points
           min-area Long/MAX_VALUE
           best-points nil
           seconds 0]
      (let [moved-points (move-points points)
            bbox (bounding-box moved-points)
            current-area (area bbox)]
        (if (< current-area min-area)
          (recur moved-points current-area moved-points (inc seconds))
          (do
            (println "Message appears at second:" (dec seconds))
            (print-points best-points)))))))

(-main)