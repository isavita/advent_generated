
(ns air-duct-spelunking
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-map [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (vec (map vec lines))
        locations (reduce (fn [acc row-idx]
                            (reduce (fn [acc col-idx]
                                      (let [c (get-in grid [row-idx col-idx])]
                                        (if (Character/isDigit c)
                                          (assoc acc (Integer/parseInt (str c)) [row-idx col-idx])
                                          acc)))
                                    acc
                                    (range width)))
                          {}
                          (range height))]
    {:grid grid
     :locations locations
     :height height
     :width width}))

(defn neighbors [[row col] {:keys [grid height width]}]
  (let [candidates [[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]]
        valid-coords (fn [[r c]] (and (>= r 0) (< r height) (>= c 0) (< c width)))]
    (filter (fn [[r c]] (not= \# (get-in grid [r c])))
            (filter valid-coords candidates))))

(defn bfs [start end {:keys [grid] :as map-data}]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{start}]
    (if (empty? queue)
      nil ; No path found
      (let [[curr dist] (peek queue)
            queue (pop queue)]
        (if (= curr end)
          dist
          (let [new-neighbors (remove visited (neighbors curr map-data))]
            (recur (reduce conj queue (map (fn [n] [n (inc dist)]) new-neighbors))
                   (into visited new-neighbors))))))))

(defn calculate-distances [map-data]
  (let [locations (:locations map-data)
        location-ids (keys locations)]
    (reduce (fn [acc a]
              (reduce (fn [acc b]
                        (if (>= b a)
                          acc
                          (let [dist (bfs (get locations a) (get locations b) map-data)]
                            (assoc acc [a b] dist))))
                      acc
                      location-ids))
            {}
            location-ids)))

(defn shortest-path [distances locations return-to-start?]
  (let [non-zero-locations (disj (set (keys locations)) 0)
        all-permutations (->> non-zero-locations
                              (set/permutations)
                              (map (fn [perm] (cons 0 perm))))
        path-lengths (map (fn [path]
                             (reduce (fn [sum [a b]]
                                       (let [key (sort [a b])]
                                         (+ sum (or (distances key) (distances (reverse key))))))
                                     0
                                     (partition 2 1 path)))
                           all-permutations)]
    (if return-to-start?
      (reduce min (map (fn [len path]
                         (+ len (or (distances (sort [(last path) 0])) (distances (sort [0 (last path)])))))
                       path-lengths all-permutations))
      (apply min path-lengths))))

(defn solve [input return-to-start?]
  (let [map-data (parse-map input)
        distances (calculate-distances map-data)]
    (shortest-path distances (:locations map-data) return-to-start?)))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part 1:" (solve input false))
    (println "Part 2:" (solve input true))))
