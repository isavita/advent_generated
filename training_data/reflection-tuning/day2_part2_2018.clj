(ns day2
  (:require [clojure.string :as str]))

(defn diff-count [s1 s2]
  (count (remove true? (map = s1 s2))))

(defn common-letters [s1 s2]
  (apply str (map first (filter #(apply = %) (map vector s1 s2)))))

(defn find-matching-ids [ids]
  (first
   (keep (fn [[id1 id2]]
           (when (= 1 (diff-count id1 id2))
             (common-letters id1 id2)))
         (for [id1 ids
               id2 ids
               :when (not= id1 id2)]
           [id1 id2]))))

(defn solve-part2 [input-file]
  (let [ids (str/split-lines (slurp input-file))]
    (find-matching-ids ids)))

(println (solve-part2 "input.txt"))
