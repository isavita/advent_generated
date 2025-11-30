
(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main [& _]
  (->> (slurp "input.txt")
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))
       (reduce +)
       (println)))

(-main)
