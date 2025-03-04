
(ns day25
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    {:grid (vec (map vec lines))
     :height height
     :width width}))

(defn move-east [grid width height]
  (let [new-grid (atom (vec (map vec grid)))]
    (doseq [y (range height)
            x (range width)]
      (when (= \> (get-in grid [y x]))
        (let [next-x (mod (inc x) width)]
          (when (= \. (get-in grid [y next-x]))
            (swap! new-grid assoc-in [y next-x] \>)
            (swap! new-grid assoc-in [y x] \.)))))
    @new-grid))

(defn move-south [grid width height]
  (let [new-grid (atom (vec (map vec grid)))]
    (doseq [y (range height)
            x (range width)]
      (when (= \v (get-in grid [y x]))
        (let [next-y (mod (inc y) height)]
          (when (= \. (get-in grid [next-y x]))
            (swap! new-grid assoc-in [next-y x] \v)
            (swap! new-grid assoc-in [y x] \.)))))
    @new-grid))

(defn step [{:keys [grid height width]}]
  (let [grid-after-east (move-east grid width height)
        grid-after-south (move-south grid-after-east width height)]
    {:grid grid-after-south
     :height height
     :width width}))


(defn solve [input]
  (loop [state (parse-input input)
         steps 0]
    (let [next-state (step state)]
      (if (= (:grid state) (:grid next-state))
        (inc steps)
        (recur next-state (inc steps))))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
