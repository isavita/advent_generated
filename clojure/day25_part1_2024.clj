
(ns towel
  (:require [clojure.string :as str]))

(defn parse-lock [block]
  (vec (for [c (range 5)]
         (count (take-while #(= % \#) (map #(nth % c) (rest block)))))))

(defn parse-key [block]
  (vec (for [c (range 5)]
         (count (take-while #(= % \#) (map #(nth % c) (reverse (butlast block))))))))

(defn fits? [lock key]
  (every? #(<= (+ (lock %) (key %)) 5) (range 5)))

(defn classify [block]
  (if (every? #(= % \#) (take 5 (first block)))
    [:lock (parse-lock block)]
    [:key  (parse-key block)]))

(defn -main [& _]
  (let [lines (->> (slurp "input.txt")
                   str/split-lines
                   (remove str/blank?))
        blocks (partition 7 lines)
        [locks keys] (reduce (fn [[ls ks] block]
                               (let [[type pins] (classify block)]
                                 (case type
                                   :lock [(conj ls pins) ks]
                                   :key  [ls (conj ks pins)])))
                             [[] []] blocks)]
    (println (count (for [l locks k keys :when (fits? l k)] [l k])))))

(-main)
