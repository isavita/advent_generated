
(ns firewall-rules
  (:require [clojure.string :as str]))

(defn parse-ranges [lines]
  (map (fn [line]
         (let [[start end] (map read-string (str/split line #"-"))]
           [start end]))
       lines))

(defn merge-ranges [ranges]
  (let [sorted (sort-by first ranges)]
    (reduce (fn [acc [start end]]
              (if (empty? acc)
                [[start end]]
                (let [[prev-start prev-end] (last acc)]
                  (if (>= start (inc prev-end))
                    (conj acc [start end])
                    (conj (vec (butlast acc)) [prev-start (max prev-end end)])))))
            []
            sorted)))

(defn find-lowest-unblocked [merged-ranges]
  (loop [current-ip 0
        ranges merged-ranges]
    (if (empty? ranges)
      current-ip
      (let [[start end] (first ranges)]
        (if (< current-ip start)
          current-ip
          (recur (max (inc end) current-ip) (rest ranges)))))))

(defn count-allowed-ips [merged-ranges]
  (let [total-ips 4294967296]
    (reduce (fn [acc [start end]]
              (- acc (- (inc end) start)))
            total-ips
            merged-ranges)))

(defn -main []
  (let [lines (slurp "input.txt")
        ranges (parse-ranges (str/split-lines lines))
        merged-ranges (merge-ranges ranges)
        lowest-unblocked (find-lowest-unblocked merged-ranges)
        allowed-count (count-allowed-ips merged-ranges)]
    (println "Lowest unblocked IP:" lowest-unblocked)
    (println "Total allowed IPs:" allowed-count)))

(-main)
