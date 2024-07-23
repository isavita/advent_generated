
(ns four-dimensional-adventure
  (:require [clojure.java.io :as io]))

(defn manhattan-distance [p1 p2]
  (reduce + (map #(Math/abs (- %1 %2)) p1 p2)))

(defn connected? [p1 p2]
  (<= (manhattan-distance p1 p2) 3))

(defn find-constellations [points]
  (let [visited (atom #{})]
    (loop [remaining points
           constellations 0]
      (if (empty? remaining)
        constellations
        (let [current (first remaining)
              to-visit (atom [current])]
          (while (not (empty? @to-visit))
            (let [point (first @to-visit)]
              (swap! to-visit rest)
              (when-not (contains? @visited point)
                (swap! visited conj point)
                (doseq [next (rest remaining)]
                  (when (connected? point next)
                    (swap! to-visit conj next))))))
          (recur (remove #(contains? @visited %) remaining)
                 (inc constellations)))))))

(defn read-input [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map #(vec (map read-string (clojure.string/split % #","))) (line-seq rdr)))))

(defn -main []
  (let [points (read-input "input.txt")
        constellations (find-constellations points)]
    (println "Number of constellations:" constellations)))

(-main)
