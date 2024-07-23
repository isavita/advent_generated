
(ns adapter-array
  (:require [clojure.java.io :as io]))

(defn read-adapters [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map #(Integer/parseInt %) (line-seq rdr)))))

(defn calculate-differences [adapters]
  (let [sorted-adapters (sort (conj adapters (+ 3 (apply max adapters)) 0))
        differences (map - (rest sorted-adapters) sorted-adapters)]
    (frequencies differences)))

(defn main []
  (let [adapters (read-adapters "input.txt")
        differences (calculate-differences adapters)
        one-jolt-diff (get differences 1 0)
        three-jolt-diff (get differences 3 0)]
    (* one-jolt-diff three-jolt-diff)))

(println (main))
