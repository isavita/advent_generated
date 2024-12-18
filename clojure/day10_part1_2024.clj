
(ns day10.core
  (:require [clojure.string :as str]))

(defn parse-map [input]
  (->> (str/split-lines input)
       (mapv #(mapv (comp read-string str) %))))

(defn find-trailheads [heightmap]
  (let [rows (count heightmap)
        cols (count (first heightmap))]
    (for [r (range rows)
          c (range cols)
          :when (= 0 (get-in heightmap [r c]))]
      [r c])))

(defn find-reachable-nines [heightmap trailhead]
  (let [rows (count heightmap)
        cols (count (first heightmap))
        directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY [trailhead 0])
           visited #{}
           nines #{}]
      (if (empty? q)
        nines
        (let [[[r c] height] (peek q)
              q (pop q)]
          (if (visited [r c])
            (recur q visited nines)
            (let [visited (conj visited [r c])
                  nines (if (= 9 height) (conj nines [r c]) nines)]
              (recur (reduce (fn [q [dr dc]]
                               (let [nr (+ r dr)
                                     nc (+ c dc)]
                                 (if (and (>= nr 0) (< nr rows)
                                          (>= nc 0) (< nc cols)
                                          (= (inc height) (get-in heightmap [nr nc])))
                                   (conj q [[nr nc] (inc height)])
                                   q)))
                             q
                             directions)
                     visited
                     nines))))))))

(defn calculate-score [heightmap trailhead]
  (count (find-reachable-nines heightmap trailhead)))

(defn solve [input]
  (let [heightmap (parse-map input)
        trailheads (find-trailheads heightmap)]
    (->> trailheads
         (map #(calculate-score heightmap %))
         (reduce +))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
