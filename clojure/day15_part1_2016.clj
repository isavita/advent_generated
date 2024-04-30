(ns disc-align
  (:require [clojure.java.io :as io]
             [clojure.string :as str]))

(defn parse-disc [line]
  (let [[_ total-positions start-position] (re-matches #"Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)." line)]
    {:total-positions (Integer/parseInt total-positions)
     :start-position (Integer/parseInt start-position)}))

(defn check-discs [discs time]
  (every? #(= 0 (mod (+ (:start-position %) time (inc (.indexOf discs %))) (:total-positions %))) discs))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [discs (map parse-disc (line-seq rdr))]
      (loop [time 0]
        (if (check-discs discs time)
          (println time)
          (recur (inc time)))))))

(-main)