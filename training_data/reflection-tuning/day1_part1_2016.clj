(ns easter-bunny-hq
  (:require [clojure.string :as str]))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn turn [current-dir turn-dir]
  (let [current-index (.indexOf directions current-dir)
        new-index (mod (+ current-index (if (= turn-dir \L) -1 1)) 4)]
    (nth directions new-index)))

(defn move [{:keys [pos dir] :as state} [turn-dir steps]]
  (let [new-dir (turn dir turn-dir)
        new-pos (mapv + pos (map #(* steps %) new-dir))]
    (assoc state :pos new-pos :dir new-dir)))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solve-puzzle [input]
  (let [instructions (map (juxt first #(Integer/parseInt (subs % 1)))
                          (str/split (str/trim input) #", "))
        final-state (reduce move {:pos [0 0] :dir [0 1]} instructions)]
    (manhattan-distance (:pos final-state))))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve-puzzle input)]
    (println result)))

(-main)
