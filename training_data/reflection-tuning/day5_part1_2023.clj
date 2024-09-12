(ns day5.core
  (:require [clojure.string :as str]))

(defn parse-numbers [s]
  (map #(Long/parseLong %) (re-seq #"\d+" s)))

(defn parse-input [input]
  (let [[seeds & maps] (str/split input #"\n\n")]
    {:seeds (parse-numbers seeds)
     :maps (map (fn [m]
                  (map parse-numbers (rest (str/split-lines m))))
                maps)}))

(defn apply-mapping [num [dest src len]]
  (if (and (>= num src) (< num (+ src len)))
    (+ dest (- num src))
    num))

(defn apply-map [num mappings]
  (reduce (fn [n m]
            (let [result (apply-mapping n m)]
              (if (not= result n)
                (reduced result)
                result)))
          num
          mappings))

(defn seed-to-location [seed maps]
  (reduce apply-map seed maps))

(defn solve [input]
  (let [{:keys [seeds maps]} (parse-input input)]
    (apply min (map #(seed-to-location % maps) seeds))))

(defn -main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(-main)
