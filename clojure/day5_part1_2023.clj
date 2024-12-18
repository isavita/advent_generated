
(ns day5
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [[seeds & maps] (str/split input #"\n\n")
        parsed-seeds (map parse-long (re-seq #"\d+" seeds))
        parsed-maps (map (fn [m]
                           (let [[_ & lines] (str/split m #"\n")
                                 parsed-lines (map (fn [line] (map parse-long (re-seq #"\d+" line))) lines)]
                             parsed-lines))
                         maps)]
    {:seeds parsed-seeds :maps parsed-maps}))

(defn apply-map [value mapping]
  (or (some (fn [[dest-start source-start range-len]]
              (when (<= source-start value (+ source-start range-len -1))
                (+ dest-start (- value source-start))))
            mapping)
      value))

(defn seed-to-location [seed maps]
  (reduce apply-map seed maps))

(defn solve [input]
  (let [{:keys [seeds maps]} (parse-input input)]
    (->> seeds
         (map #(seed-to-location % maps))
         (apply min))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
