(ns day1-solution
  (:require [clojure.string :as str]))

(defn extract-calibration-value [line]
  (let [digits (re-seq #"\d" line)]
    (if (seq digits)
      (Integer/parseInt (str (first digits) (last digits)))
      0)))

(defn solve-calibration [input-file]
  (->> (slurp input-file)
       (str/split-lines)
       (map extract-calibration-value)
       (reduce +)))

(defn -main []
  (println (solve-calibration "input.txt")))

(-main)
