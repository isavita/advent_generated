
(ns ram-run
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-coords [line]
  (let [[x y] (str/split line #",")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-coords)))

(defn build-grid [coords size]
  (let [grid (vec (repeat size (vec (repeat size false))))]
    (reduce (fn [g [x y]]
              (if (and (>= x 0) (< x size) (>= y 0) (< y size))
                (assoc-in g [y x] true)
                g))
            grid
            coords)))

(defn get-neighbors [[x y] size]
  (let [candidates [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]]
    (filter (fn [[nx ny]]
              (and (>= nx 0) (< nx size) (>= ny 0) (< ny size)))
            candidates)))

(defn solve [coords size]
  (let [corrupted-grid (build-grid coords size)
        start [0 0]
        end [(dec size) (dec size)]
        q (priority-map start 0)
        visited #{}]
    (loop [q q visited visited]
      (if (empty? q)
        nil ; No path found
        (let [[[x y] dist] (peek q)
              q (pop q)]
          (if (= [x y] end)
            dist ; Found the shortest path
            (if (contains? visited [x y])
              (recur q visited)
              (let [neighbors (get-neighbors [x y] size)
                    unvisited-neighbors (remove visited neighbors)
                    safe-neighbors (filter (fn [[nx ny]] (not (get-in corrupted-grid [ny nx]))) unvisited-neighbors)
                    updated-q (reduce (fn [q [nx ny]] (assoc q [nx ny] (inc dist))) q safe-neighbors)]
                (recur updated-q (conj visited [x y]))))))))))

(defn -main [& args]
  (let [filename (or (first args) "input.txt")
        coords (take 1024 (read-input filename))
        size 71
        result (solve coords size)]
    (println result)))

(comment
  (-main)
  )
