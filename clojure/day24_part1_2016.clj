
(ns day24
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    {:grid (into {} (for [y (range height)
                          x (range width)
                          :let [c (get-in lines [y x])]]
                      [[x y] c]))
     :width width
     :height height}))

(defn find-locations [grid]
  (->> (for [[[x y] c] grid
             :when (Character/isDigit c)]
         [(Character/digit c 10) [x y]])
       (into {})))

(defn neighbors [[x y] width height]
  (filter (fn [[nx ny]]
            (and (>= nx 0) (< nx width)
                 (>= ny 0) (< ny height)))
          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn bfs [grid start end width height]
  (loop [q (clojure.lang.PersistentQueue/EMPTY)
         visited #{}
         dist {start 0}]
    (if (empty? q)
      -1 ; Or nil, indicating no path found.  Returning -1 to be explicit.
      (let [curr (peek q)
            q (pop q)]
        (if (= curr end)
          (dist curr)
          (let [valid-neighbors (->> (neighbors curr width height)
                                     (filter #(not= \# ((:grid grid) %)))
                                     (filter #(not (contains? visited %))))
                new-visited (into visited valid-neighbors)
                new-dist (reduce (fn [d n] (assoc d n (inc (dist curr))))
                                 dist valid-neighbors)
                new-q (reduce conj q valid-neighbors)]
            (recur new-q new-visited new-dist)))))))

(defn solve [input]
  (let [{:keys [grid width height]} (parse-input input)
        locations (find-locations grid)
        location-keys (keys locations)
        location-pairs (for [a location-keys
                              b location-keys
                              :when (not= a b)]
                          [[a b] (bfs grid (locations a) (locations b) width height)])
        distances (into {} location-pairs)
        ]
       (let [perms (if (seq (rest location-keys))
                     (->> (rest location-keys)
                          (clojure.math.combinatorics/permutations))
                     [[]]) ]   
    (->> perms
        (map (fn [p]
               (cons 0 p)))
        (map (fn [path]
               (reduce (fn [acc [a b]]
                         (+ acc (distances [a b])))
                       0
                       (partition 2 1 path))))
        (apply min)))))

(defn -main [& args]
  (let [input (slurp "input.txt")
        result (solve input)]
    (println result)))
