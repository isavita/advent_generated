(ns day5
  (:require [clojure.string :as str]))

(defn parse-numbers [s]
  (map parse-long (re-seq #"\d+" s)))

(defn parse-input [input]
  (let [[seeds & maps] (str/split input #"\n\n")]
    {:seeds (parse-numbers seeds)
     :maps (map (fn [m]
                  (let [[_ & ranges] (str/split-lines m)]
                    (map parse-numbers ranges)))
                maps)}))

(defn apply-mapping [ranges mapping]
  (loop [ranges ranges
         mapping mapping
         result []]
    (if (empty? ranges)
      result
      (let [[start end] (first ranges)
            overlaps (filter (fn [[_ source length]]
                               (and (< start (+ source length))
                                    (> end source)))
                             mapping)]
        (if (empty? overlaps)
          (recur (rest ranges) mapping (conj result (first ranges)))
          (let [[dest source length] (first overlaps)
                before (when (< start source)
                         [start (min end source)])
                middle [(max start source)
                        (min end (+ source length))]
                after (when (> end (+ source length))
                        [(max start (+ source length)) end])
                new-middle [(+ dest (- (first middle) source))
                             (+ dest (- (second middle) source))]]
            (recur (concat (rest ranges)
                           (filter identity [before after]))
                   mapping
                   (conj result new-middle))))))))

(defn process-ranges [initial-ranges maps]
  (reduce (fn [ranges mapping]
            (apply-mapping ranges mapping))
          initial-ranges
          maps))

(defn solve-part1 [{:keys [seeds maps]}]
  (let [initial-ranges (map (fn [seed] [seed (inc seed)]) seeds)]
    (apply min (map first (process-ranges initial-ranges maps)))))

(defn solve-part2 [{:keys [seeds maps]}]
  (let [initial-ranges (partition 2 seeds)
        ranges (map (fn [[start length]] [start (+ start length)]) initial-ranges)]
    (apply min (map first (process-ranges ranges maps)))))

(defn solve [input]
  (let [data (parse-input input)]
    [(solve-part1 data)
     (solve-part2 data)]))
