
(ns solution
  (:require [clojure.java.io :as io]))

(defn count-twos-and-threes [id]
  (let [char-count (frequencies id)]
    (let [has-twos (some #(= 2 %) (vals char-count))
          has-threes (some #(= 3 %) (vals char-count))]
      [has-twos has-threes])))

(defn -main []
  (let [file (io/reader "input.txt")]
    (let [lines (line-seq file)
          counts (reduce (fn [[twos threes] id]
                           (let [[twos? threes?] (count-twos-and-threes id)]
                             [(if twos? (inc twos) twos)
                              (if threes? (inc threes) threes)]))
                         [0 0]
                         lines)]
      (let [checksum (* (first counts) (second counts))]
        (println checksum)))))

(-main)
