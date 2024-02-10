
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input []
  (-> (slurp "input.txt")
      (str/split-lines)))

(defrecord Coordinate4D [x y z w])

(defn simulate-cycle-4d [active-cubes]
  (let [new-active-cubes (atom {})
        neighbor-counts (atom {})]
    (doseq [[coord _] active-cubes
            dw (range -1 2)
            dz (range -1 2)
            dy (range -1 2)
            dx (range -1 2)
            :when (not (and (zero? dw) (zero? dz) (zero? dy) (zero? dx)))]
      (let [neighbor (->Coordinate4D (+ (:x coord) dx)
                                     (+ (:y coord) dy)
                                     (+ (:z coord) dz)
                                     (+ (:w coord) dw))]
        (swap! neighbor-counts update neighbor (fnil inc 0))))
    (doseq [[coord count] @neighbor-counts
            :when (or (= count 3) (and (= count 2) (active-cubes coord)))]
      (swap! new-active-cubes assoc coord true))
    @new-active-cubes))

(defn -main []
  (let [initial-state (read-input)
        active-cubes (atom {})]
    (doseq [[line y] (map vector initial-state (range))]
      (doseq [[char x] (map vector line (range))]
        (when (= char \#)
          (swap! active-cubes assoc (->Coordinate4D x y 0 0) true))))
    (dotimes [cycle 6]
      (reset! active-cubes (simulate-cycle-4d @active-cubes)))
    (println (count @active-cubes))))

(-main)
