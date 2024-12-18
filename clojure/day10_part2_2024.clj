
(ns day10
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(mapv (comp read-string str) %))))

(defn find-trailheads [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= 0 (get-in grid [y x]))]
    [y x]))

(defn neighbors [grid [y x]]
  (let [h (count grid)
        w (count (first grid))]
    (filter (fn [[ny nx]]
              (and (>= ny 0) (< ny h)
                   (>= nx 0) (< nx w)))
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn find-trails [grid trailhead]
  (letfn [(dfs [path visited]
            (let [curr (last path)
                  curr-val (get-in grid curr)]
              (if (= 9 curr-val)
                [path]
                (->> (neighbors grid curr)
                     (filter #(and (not (visited %))
                                   (= (inc curr-val) (get-in grid %))))
                     (mapcat #(dfs (conj path %) (conj visited %)))
                     (into [])))))]
    (dfs [trailhead] #{trailhead})))

(defn part1 [grid]
  (let [trailheads (find-trailheads grid)
        trails (map #(find-trails grid %) trailheads)
        scores (map (fn [trail-set]
                      (count (set (filter #(= 9 (get-in grid %))
                                          (mapcat identity trail-set)))))
                    trails)]
    (reduce + scores)))

(defn part2 [grid]
  (let [trailheads (find-trailheads grid)
        trails (map #(find-trails grid %) trailheads)
        ratings (map #(count (set %)) trails)]
    (reduce + ratings)))

(let [input (slurp "input.txt")
      grid (parse-input input)]
  (println "Part 1:" (part1 grid))
  (println "Part 2:" (part2 grid)))
