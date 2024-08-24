(ns day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-moons [input]
  (map #(let [[_ x y z] (re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>" %)]
           {:position {:x (Integer/parseInt x) :y (Integer/parseInt y) :z (Integer/parseInt z)}
            :velocity {:x 0 :y 0 :z 0}})
       (str/split-lines input)))

(defn apply-gravity [moons]
  (for [moon moons]
    (let [velocity (reduce (fn [v other-moon]
                             (assoc v
                               :x (+ (:x v) (compare (:x (:position other-moon)) (:x (:position moon))))
                               :y (+ (:y v) (compare (:y (:position other-moon)) (:y (:position moon))))
                               :z (+ (:z v) (compare (:z (:position other-moon)) (:z (:position moon))))))
                           (:velocity moon)
                           (remove #(= % moon) moons))]
      (assoc moon :velocity velocity))))

(defn apply-velocity [moons]
  (map #(update % :position (fn [p] (assoc p
                                           :x (+ (:x p) (:x (:velocity %)))
                                           :y (+ (:y p) (:y (:velocity %)))
                                           :z (+ (:z p) (:z (:velocity %)))))) moons))

(defn simulate-step [moons]
  (-> moons
      apply-gravity
      apply-velocity))

(defn total-energy [moons]
  (reduce + (map (fn [moon]
                   (* (reduce + (map #(Math/abs %) (vals (:position moon))))
                      (reduce + (map #(Math/abs %) (vals (:velocity moon))))))
                 moons)))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [moons (parse-moons (slurp rdr))]
      (let [final-moons (nth (iterate simulate-step moons) 1000)]
        (println (total-energy final-moons))))))

(-main)