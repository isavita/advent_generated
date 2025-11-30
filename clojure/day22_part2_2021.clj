
(ns reboot
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [l]
  (let [[_ on? xs xe ys ye zs ze]
        (re-matches #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" l)]
    {:isOn (= on? "on")
     :x1 (parse-long xs) :x2 (parse-long xe)
     :y1 (parse-long ys) :y2 (parse-long ye)
     :z1 (parse-long zs) :z2 (parse-long ze)}))

(defn parse-input [txt]
  (mapv parse-line (str/split-lines (str/trim txt))))

(defn intersect [a b]
  (let [x1 (max (:x1 a) (:x1 b)) x2 (min (:x2 a) (:x2 b))
        y1 (max (:y1 a) (:y1 b)) y2 (min (:y2 a) (:y2 b))
        z1 (max (:z1 a) (:z1 b)) z2 (min (:z2 a) (:z2 b))]
    (when (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
      {:isOn (not (:isOn a))
       :x1 x1 :x2 x2 :y1 y1 :y2 y2 :z1 z1 :z2 z2})))

(defn volume [{:keys [x1 x2 y1 y2 z1 z2 isOn]}]
  (* (if isOn 1 -1)
     (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))

(defn solve [cubes]
  (loop [[c & cs] cubes final []]
    (if-not c
      (reduce + (map volume final))
      (let [adds (keep #(intersect % c) final)]
        (recur cs (into final (if (:isOn c) (conj adds c) adds)))))))

(defn -main [& _]
  (println (solve (parse-input (slurp "input.txt")))))

(-main)
