
(ns fresh-check
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-range [line]
  (let [[a b] (str/split line #"-")]
    [(Long/parseUnsignedLong a) (Long/parseUnsignedLong b)]))

(defn parse-id [line]
  (Long/parseUnsignedLong line))

(defn in-range? [n [a b]]
  (and (>= n a) (<= n b)))

(defn -main [& _]
  (let [lines (line-seq (io/reader "input.txt"))
        ranges-end (.indexOf lines "")
        ranges (mapv parse-range (take ranges-end lines))
        ids (drop (inc ranges-end) lines)
        fresh (count (filter #(some (partial in-range? (parse-id %)) ranges) ids))]
    (println fresh)))

(-main)
