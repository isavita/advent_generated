
(ns chiton
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-grid [file]
  (with-open [rdr (io/reader file)]
    (vec (mapv (fn [l] (mapv #(- (int %) 48) l)) (line-seq rdr)))))

(defn extend-grid [grid]
  (let [rows (count grid)
        cols (count (grid 0))]
    (vec (for [i (range (* rows 5))]
           (vec (for [j (range (* cols 5))]
                  (inc (mod (+ (get-in grid [(mod i rows) (mod j cols)])
                               (quot i rows) (quot j cols) -1) 9))))))))

(defn dijkstra [grid]
  (let [rows (count grid)
        cols (count (grid 0))
        target [(dec rows) (dec cols)]
        dist (atom (zipmap (for [i (range rows) j (range cols)] [i j])
                           (repeat Integer/MAX_VALUE)))
        pq (java.util.PriorityQueue.
            (fn [[_ d1] [_ d2]] (compare d1 d2)))]
    (.add pq [[0 0] 0])
    (swap! dist assoc [0 0] 0)
    (loop []
      (when-let [[[x y] d] (when (pos? (.size pq)) (.poll pq))]
        (when-not (= [x y] target)
          (doseq [[dx dy] [[1 0] [0 1] [-1 0] [0 -1]]
                  :let [nx (+ x dx) ny (+ y dy)]
                  :when (and (< -1 nx rows) (< -1 ny cols))]
            (let [nd (+ d (get-in grid [nx ny]))]
              (when (< nd (get @dist [nx ny]))
                (swap! dist assoc [nx ny] nd)
                (.add pq [[nx ny] nd])))))
          (recur)))
    (get @dist target)))

(defn -main [& _]
  (let [grid (read-grid "input.txt")]
    (println (dijkstra (extend-grid grid)))))

(-main)
