
(ns wire-crossing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-move [move]
  (let [dir (first move)
        dist (Integer. (subs move 1))]
    [dir dist]))

(defn get-points-with-steps [path]
  (let [moves (map parse-move (str/split path #","))
        points (atom {})
        current (atom [0 0])
        steps (atom 0)]
    (doseq [[dir dist] moves]
      (dotimes [_ dist]
        (swap! steps inc)
        (case dir
          \U (swap! current update 1 inc)
          \D (swap! current update 1 dec)
          \L (swap! current update 0 dec)
          \R (swap! current update 0 inc))
        (let [point @current]
          (when-not (contains? @points point)
            (swap! points assoc point @steps)))))
    @points))

(defn main []
  (let [lines (str/split (slurp "input.txt") #"\n")
        wire1 (get-points-with-steps (first lines))
        wire2 (get-points-with-steps (second lines))]
    (println (apply min (for [[p steps1] wire1
                               :let [steps2 (get wire2 p)]
                               :when steps2]
                           (+ steps1 steps2))))))

(main)
