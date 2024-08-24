(ns memory-reallocation
  (:require [clojure.string :as str]))

(defn redistribute [banks]
  (let [max-blocks (apply max banks)
        max-index (.indexOf banks max-blocks)]
    (loop [banks (assoc banks max-index 0)
           blocks max-blocks
           index (mod (inc max-index) (count banks))]
      (if (zero? blocks)
        banks
        (recur (assoc banks index (inc (banks index)))
               (dec blocks)
               (mod (inc index) (count banks)))))))

(defn solve [input]
  (let [banks (mapv #(Integer/parseInt %) (str/split input #"\s+"))
        seen-configs (atom #{})
        cycles (atom 0)]
    (loop [current-banks banks]
      (if (@seen-configs current-banks)
        @cycles
        (do
          (swap! seen-configs conj current-banks)
          (swap! cycles inc)
          (recur (redistribute current-banks)))))))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [input (slurp rdr)]
      (println (solve input)))))

(-main)