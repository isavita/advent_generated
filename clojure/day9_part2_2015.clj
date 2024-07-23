
(ns longest-route
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-and-parse-input [filename]
  (with-open [rdr (io/reader filename)]
    (reduce (fn [distances line]
              (let [[from _ to _ dist] (str/split line #"\s+")]
                (let [distance (Integer. dist)]
                  (-> distances
                      (update-in [from to] (fnil + 0) distance)
                      (update-in [to from] (fnil + 0) distance)))))
            {}
            (line-seq rdr))))

(defn get-unique-locations [distances]
  (set (keys distances)))

(defn calculate-route-distance [route distances]
  (reduce + 0 (map (fn [[from to]] (get-in distances [from to])) (partition 2 1 route))))

(defn permute [coll]
  (if (empty? coll)
    (list ())
    (for [x coll
          xs (permute (remove #{x} coll))]
      (cons x xs))))

(defn find-longest-route [locations distances]
  (apply max (map #(calculate-route-distance % distances) (permute locations))))

(defn -main []
  (let [distances (read-and-parse-input "input.txt")
        locations (get-unique-locations distances)]
    (println (find-longest-route locations distances))))

(-main)
