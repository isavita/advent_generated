
(ns almanac
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-longs [s]
  (mapv #(Long/parseLong %) (re-seq #"\d+" s)))

(defn make-range-maps [lines]
  (for [ln lines :when (re-find #"\d" ln)]
    (let [[dst src len] (parse-longs ln)]
      {:src src :dst dst :len len})))

(defn reverse-map [n ranges]
  (or (some (fn [{:keys [src dst len]}]
              (when (<= dst n (dec (+ dst len)))
                (+ src (- n dst))))
            ranges)
      n))

(defn in-seed-ranges? [n seed-ranges]
  (some (fn [[start len]] (<= start n (dec (+ start len)))) seed-ranges))

(defn -main []
  (let [blocks (str/split (slurp "input.txt") #"\r?\n\r?\n")
        seeds (parse-longs (first blocks))
        seed-ranges (partition 2 seeds)
        maps (mapv #(make-range-maps (str/split-lines %)) (rest blocks))]
    (loop [location 0]
      (let [seed (reduce reverse-map location (reverse maps))]
        (if (in-seed-ranges? seed seed-ranges)
          (println location)
          (recur (inc location)))))))

(-main)
