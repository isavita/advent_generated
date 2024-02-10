(ns solution
  (:require [clojure.java.io :as io]))

(defn read-input []
  (slurp "input.txt"))

(defn solve []
  (let [directions (read-input)
        visited-houses (atom {})]
    (let [x-santa (atom 0)
          y-santa (atom 0)
          x-robo (atom 0)
          y-robo (atom 0)
          is-santa-turn (atom true)]
      (swap! visited-houses assoc [(vector 2) @x-santa @y-santa] true)
      
      (doseq [dir directions]
        (let [x (if @is-santa-turn x-santa x-robo)
              y (if @is-santa-turn y-santa y-robo)]
          (case dir
            \^ (swap! y inc) ; Move north
            \v (swap! y dec) ; Move south
            \> (swap! x inc) ; Move east
            \< (swap! x dec)) ; Move west
          
          (swap! visited-houses assoc [(vector 2) @x @y] true)
          (swap! is-santa-turn not)))
      
      (println (count @visited-houses)))))

(solve)