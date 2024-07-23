
(ns sand-slabs
  (:require [clojure.string :as str]))

(defn parse-bricks [lines]
  (map (fn [line]
         (let [[start end] (str/split line #"~")
               [x1 y1 z1] (map #(Integer/parseInt %) (str/split start #","))
               [x2 y2 z2] (map #(Integer/parseInt %) (str/split end #","))]
           (if (= x1 x2)
             (if (= y1 y2)
               (let [z-min (min z1 z2)
                     z-max (max z1 z2)]
                 (for [z (range z-min (inc z-max))]
                   [x1 y1 z]))
               (let [x-min (min x1 x2)
                     x-max (max x1 x2)]
                 (for [x (range x-min (inc x-max))]
                   [x y1 z1])))
             (let [y-min (min y1 y2)
                   y-max (max y1 y2)]
               (for [y (range y-min (inc y-max))]
                 [x1 y z1])))))
       lines))

(defn settle-bricks [bricks]
  (let [brick-set (set (apply concat bricks))
        settled (atom brick-set)]
    (doseq [z (range 1 (inc (apply max (map #(last %) (apply concat bricks)))))]
      (doseq [x (range 0 10)
              y (range 0 10)
              :when (not (settled @settled))
              :let [pos [x y z]]
              :when (not (contains? @settled pos))]
        (when (some #(and (contains? @settled %) (>= z (last %))) (filter #(= x (first %)) (filter #(= y (second %)) @settled)))
          (swap! settled conj pos))))
    @settled))

(defn can-disintegrate? [brick settled]
  (let [above (map #(vector (first brick) (second brick) (inc (last brick))) (filter #(= (first brick) (first %)) settled))]
    (not (some #(not (contains? settled %)) above))))

(defn count-safe-disintegrations [settled bricks]
  (count (filter #(can-disintegrate? % settled) bricks)))

(defn main []
  (let [lines (str/split-lines (slurp "input.txt"))
        bricks (parse-bricks lines)
        settled (settle-bricks bricks)]
    (println "Safe bricks to disintegrate:" (count-safe-disintegrations settled (apply concat bricks)))))

(main)
