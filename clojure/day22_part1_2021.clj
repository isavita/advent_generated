(ns reactor-reboot
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ on-off x1 x2 y1 y2 z1 z2] (re-matches #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" line)]
    {:on (= on-off "on")
     :x1 (Integer/parseInt x1)
     :x2 (Integer/parseInt x2)
     :y1 (Integer/parseInt y1)
     :y2 (Integer/parseInt y2)
     :z1 (Integer/parseInt z1)
     :z2 (Integer/parseInt z2)}))

(defn in-range [x]
  (and (<= -50 x 50)))

(defn execute-step [cubes step]
  (let [x-range (range (max -50 (:x1 step)) (inc (min 50 (:x2 step))))
        y-range (range (max -50 (:y1 step)) (inc (min 50 (:y2 step))))
        z-range (range (max -50 (:z1 step)) (inc (min 50 (:z2 step))))]
    (reduce (fn [cubes x]
              (reduce (fn [cubes y]
                        (reduce (fn [cubes z]
                                  (if (:on step)
                                    (conj cubes [x y z])
                                    (disj cubes [x y z])))
                                cubes z-range))
                      cubes y-range))
            cubes x-range)))

(defn -main []
  (let [steps (map parse-line (str/split-lines (slurp "input.txt")))]
    (println (count (reduce execute-step #{} (filter #(every? in-range [(:x1 %) (:x2 %) (:y1 %) (:y2 %) (:z1 %) (:z2 %)]) steps))))))

(-main)