
(ns universal-orbit-map
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn build-orbit-map [lines]
  (into {} (map #(let [[a b] (str/split % #"\)")] [b a]) lines)))

(defn count-orbits [orbit-map object]
  (if-let [parent (get orbit-map object)]
    (inc (count-orbits orbit-map parent))
    0))

(defn total-orbits [orbit-map]
  (transduce (map #(count-orbits orbit-map %)) + (keys orbit-map)))

(defn path-to-com [orbit-map object]
  (loop [current object path []]
    (if-let [parent (get orbit-map current)]
      (recur parent (conj path parent))
      path)))

(defn orbital-transfers [orbit-map start end]
  (let [start-path (path-to-com orbit-map start)
        end-path (path-to-com orbit-map end)
        common (some (set start-path) end-path)]
    (+ (.indexOf start-path common) (.indexOf end-path common))))

(defn -main [& _]
  (let [lines (str/split-lines (slurp "input.txt"))
        orbit-map (build-orbit-map lines)]
    (println (total-orbits orbit-map))
    (println (orbital-transfers orbit-map "YOU" "SAN"))))

(-main)
