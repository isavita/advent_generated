(ns day6
  (:require [clojure.string :as str]))

(def grid-size 1000)

(defn parse-instruction [line]
  (let [[_ action x1 y1 x2 y2] (re-find #"(.*) (\d+),(\d+) through (\d+),(\d+)" line)]
    {:action (case action
               "turn on" :on
               "turn off" :off
               "toggle" :toggle)
     :start [(Integer/parseInt x1) (Integer/parseInt y1)]
     :end [(Integer/parseInt x2) (Integer/parseInt y2)]}))

(defn update-grid [grid {:keys [action start end]}]
  (let [[x1 y1] start
        [x2 y2] end
        t-grid (transient grid)]
    (doseq [x (range x1 (inc x2))
            y (range y1 (inc y2))]
      (let [idx (+ (* y grid-size) x)]
        (assoc! t-grid idx
                (case action
                  :on (inc (get t-grid idx))
                  :off (max 0 (dec (get t-grid idx)))
                  :toggle (+ (get t-grid idx) 2)))))
    (persistent! t-grid)))

(defn process-instructions [instructions]
  (reduce update-grid (vec (repeat (* grid-size grid-size) 0)) instructions))

(defn solve [input]
  (let [instructions (map parse-instruction (str/split-lines input))
        final-grid (process-instructions instructions)]
    (reduce + final-grid)))

(defn -main [& args]
  (let [input (slurp (first args))]
    (println (solve input))))
