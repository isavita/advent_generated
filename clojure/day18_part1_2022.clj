
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-point [line]
  (let [[x y z] (mapv #(Integer/parseInt %) (str/split line #","))]
    {:x x :y y :z z}))

(defn calculate-exposed-sides [p cubes]
  (let [directions [{:x 1 :y 0 :z 0} {:x -1 :y 0 :z 0}
                    {:x 0 :y 1 :z 0} {:x 0 :y -1 :z 0}
                    {:x 0 :y 0 :z 1} {:x 0 :y 0 :z -1}]]
    (reduce (fn [exposed-sides dir]
              (let [adjacent {:x (+ (:x p) (:x dir))
                              :y (+ (:y p) (:y dir))
                              :z (+ (:z p) (:z dir))}]
                (if (some #(= adjacent %) (keys cubes))
                  (dec exposed-sides)
                  exposed-sides)))
            6
            directions)))

(let [file (io/reader "input.txt")
      cubes (reduce (fn [cubes line]
                      (let [point (parse-point line)]
                        (assoc cubes point true)))
                    {}
                    (line-seq file))]
  (println (reduce + (map #(calculate-exposed-sides % cubes) (keys cubes))))
  (.close file))
