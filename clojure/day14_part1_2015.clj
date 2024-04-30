(ns reindeer-olympics
  (:require [clojure.string :as str]))

(defn parse-reindeer [line]
  (let [[_ name speed duration rest-time] (re-matches #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\." line)]
    {:name name :speed (Integer/parseInt speed) :duration (Integer/parseInt duration) :rest-time (Integer/parseInt rest-time)}))

(defn distance-traveled [reindeer time]
  (let [cycles (quot time (+ (:duration reindeer) (:rest-time reindeer)))
        remaining-time (mod time (+ (:duration reindeer) (:rest-time reindeer)))]
    (* cycles (:speed reindeer) (:duration reindeer))
    (if (< remaining-time (:duration reindeer))
      (+ (* (:speed reindeer) remaining-time) (* cycles (:speed reindeer) (:duration reindeer)))
      (* (:speed reindeer) (:duration reindeer)))))

(defn -main []
  (let [reindeer (map parse-reindeer (str/split (slurp "input.txt") #"\n"))
        max-distance (apply max (map #(distance-traveled % 2503) reindeer))]
    (println "The winning reindeer has traveled" max-distance "km.")))

(-main)