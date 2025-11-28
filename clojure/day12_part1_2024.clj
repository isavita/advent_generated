
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn calculate-region [grid start-r start-c visited]
  (let [rows (count grid)
        cols (count (grid 0))
        ch (get-in grid [start-r start-c])
        nbs [[-1 0] [1 0] [0 -1] [0 1]]
        q (atom [[start-r start-c]])
        area (atom 0)
        peri (atom 0)]
    (swap! visited assoc-in [start-r start-c] true)
    (while (seq @q)
      (let [[r c] (first @q)]
        (swap! q rest)
        (swap! area inc)
        (doseq [[dr dc] nbs]
          (let [nr (+ r dr) nc (+ c dc)]
            (cond
              (and (< -1 nr rows) (< -1 nc cols))
              (if (not= (get-in grid [nr nc]) ch)
                (swap! peri inc)
                (when-not (get-in @visited [nr nc])
                  (swap! visited assoc-in [nr nc] true)
                  (swap! q conj [nr nc])))
              :else (swap! peri inc))))))
    [@area @peri]))

(defn solve [grid]
  (let [rows (count grid)
        cols (count (grid 0))
        visited (atom (vec (repeat rows (vec (repeat cols false)))))
        total (atom 0)]
    (doseq [r (range rows) c (range cols)]
      (when-not (get-in @visited [r c])
        (let [[a p] (calculate-region grid r c visited)]
          (swap! total + (* a p)))))
    @total))

(defn -main [& _]
  (let [grid (str/split-lines (slurp "input.txt"))]
    (println (solve grid))))

(-main)
