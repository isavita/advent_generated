(ns day6.core
  (:require [clojure.string :as str]))

(defn count-yes-answers [group]
  (->> group
       (mapcat seq)
       set
       count))

(defn solve-custom-customs [input]
  (->> (str/split input #"\n\n")
       (map #(str/split % #"\n"))
       (map count-yes-answers)
       (reduce +)))

(defn -main []
  (let [input (slurp "input.txt")
        result (solve-custom-customs input)]
    (println result)))

(-main)
