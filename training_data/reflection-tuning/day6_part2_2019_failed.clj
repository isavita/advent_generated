(ns day06
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (map #(str/split % #"\)") (str/split-lines input)))

(defn build-orbit-map [orbits]
  (reduce (fn [map [parent child]]
            (-> map
                (update parent (fnil conj #{}) child)
                (update child (fnil conj #{}) parent)))
          {}
          orbits))

(defn count-orbits [orbit-map start]
  (loop [to-visit [[start 0]]
         visited #{}
         total 0]
    (if (empty? to-visit)
      total
      (let [[node depth] (first to-visit)
            neighbors (get orbit-map node #{})]
        (recur (into (rest to-visit)
                     (map #(vector % (inc depth))
                          (remove visited neighbors)))
               (conj visited node)
               (+ total depth))))))

(defn shortest-path [orbit-map start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start])
         visited #{}]
    (when-let [path (peek queue)]
      (let [node (peek path)]
        (if (= node end)
          (dec (count path))  ; Subtract 1 to get number of transfers
          (let [neighbors (remove visited (get orbit-map node #{}))]
            (recur (into (pop queue) (map #(conj path %) neighbors))
                   (conj visited node))))))))

(defn solve-part1 [input]
  (let [orbit-map (build-orbit-map (parse-input input))]
    (count-orbits orbit-map "COM")))

(defn solve-part2 [input]
  (let [orbit-map (build-orbit-map (parse-input input))]
    (shortest-path orbit-map (get orbit-map "YOU") (get orbit-map "SAN"))))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println "Part One:" (solve-part1 input))
    (println "Part Two:" (solve-part2 input))))
