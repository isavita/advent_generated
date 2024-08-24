(ns day6
  (:require [clojure.string :as str]))

(defn manhattan-distance [p1 p2]
  (+ (Math/abs (- (first p1) (first p2)))
     (Math/abs (- (second p1) (second p2)))))

(defn read-coordinates [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map #(mapv read-string (str/split % #", ")) (line-seq rdr)))))

(defn find-bounds [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn closest-coordinate [coords x y]
  (let [distances (map #(vector % (manhattan-distance % [x y])) coords)
        sorted (sort-by second distances)]
    (if (= (second (first sorted)) (second (second sorted)))
      nil
      (first (first sorted)))))

(defn infinite-areas [coords bounds]
  (let [[min-x min-y max-x max-y] bounds
        edges (concat (for [x (range min-x (inc max-x))] [x min-y])
                      (for [x (range min-x (inc max-x))] [x max-y])
                      (for [y (range min-y (inc max-y))] [min-x y])
                      (for [y (range min-y (inc max-y))] [max-x y]))]
    (set (map #(closest-coordinate coords (first %) (second %)) edges))))

(defn largest-finite-area [coords]
  (let [bounds (find-bounds coords)
        [min-x min-y max-x max-y] bounds
        infinite (infinite-areas coords bounds)
        area-counts (atom {})]
    (doseq [x (range min-x (inc max-x))
            y (range min-y (inc max-y))]
      (let [closest (closest-coordinate coords x y)]
        (when (and closest (not (infinite closest)))
          (swap! area-counts update closest (fnil inc 0)))))
    (apply max (vals @area-counts))))

(defn -main []
  (let [coords (read-coordinates "input.txt")]
    (println (largest-finite-area coords))))

(-main)