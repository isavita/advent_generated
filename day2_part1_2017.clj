
(ns solution
  (:require [clojure.string :as str]))

(defn calculate-checksum []
  (let [data (slurp "input.txt")
        lines (str/split (str/trim data) #"\n")]
    (reduce +
            (for [line lines
                  :let [nums (str/split line #"\s+")
                        min-val (->> nums
                                     (map read-string)
                                     (apply min))
                        max-val (->> nums
                                     (map read-string)
                                     (apply max))]]
              (- max-val min-val)))))

(println (calculate-checksum))
