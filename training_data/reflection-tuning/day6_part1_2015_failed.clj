(ns day6
  (:require [clojure.string :as str]))

(def grid-size 1000)

(defn parse-coords [coord-str]
  (mapv #(Integer/parseInt %) (str/split coord-str #",")))

(defn parse-instruction [instruction]
  (let [[action & coords] (str/split instruction #" ")
        [start end] (remove #(= % "through") coords)]
    {:action (if (= action "toggle") 
               :toggle 
               (keyword (str/replace action " " "-")))
     :start (parse-coords start)
     :end (parse-coords end)}))

(defn apply-instruction [grid {:keys [action start end]}]
  (let [[x1 y1] start
        [x2 y2] end]
    (reduce (fn [g [x y]]
              (case action
                :turn-on (assoc-in g [x y] true)
                :turn-off (assoc-in g [x y] false)
                :toggle (update-in g [x y] not)))
            grid
            (for [x (range x1 (inc x2))
                  y (range y1 (inc y2))]
              [x y]))))

(defn count-lit-lights [grid]
  (count (filter true? (flatten (vals grid)))))

(defn solve [instructions]
  (let [initial-grid (vec (repeat grid-size (vec (repeat grid-size false))))
        final-grid (reduce apply-instruction 
                           initial-grid 
                           (map parse-instruction instructions))]
    (count-lit-lights final-grid)))

;; Example usage:
(def sample-instructions
  ["turn on 0,0 through 999,999"
   "toggle 0,0 through 999,0"
   "turn off 499,499 through 500,500"])

(println (solve sample-instructions))
