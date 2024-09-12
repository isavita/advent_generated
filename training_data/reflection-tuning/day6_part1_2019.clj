(ns orbit-map
  (:require [clojure.string :as str]))

(defn parse-orbit [line]
  (let [[center orbiter] (str/split line #"\)")]
    [orbiter center]))

(defn read-orbit-map [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-orbit)
       (into {})))

(defn count-orbits [orbit-map object]
  (loop [current object
         count 0]
    (if-let [parent (get orbit-map current)]
      (recur parent (inc count))
      count)))

(defn total-orbits [orbit-map]
  (->> orbit-map
       (keys)
       (map #(count-orbits orbit-map %))
       (reduce +)))

(defn -main []
  (let [orbit-map (read-orbit-map "input.txt")
        result (total-orbits orbit-map)]
    (println result)))

(-main)
