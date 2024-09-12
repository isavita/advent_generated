(ns day2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn char-counts [s]
  (frequencies s))

(defn has-n-of-any-letter? [n counts]
  (boolean (some #(= n %) (vals counts))))

(defn checksum [ids]
  (let [counts (map char-counts ids)
        twos (count (filter #(has-n-of-any-letter? 2 %) counts))
        threes (count (filter #(has-n-of-any-letter? 3 %) counts))]
    (* twos threes)))

(defn common-chars [s1 s2]
  (apply str (map first (filter #(apply = %) (map vector s1 s2)))))

(defn find-similar-ids [ids]
  (first (for [pair (combo/combinations ids 2)
               :let [[id1 id2] pair
                     common (common-chars id1 id2)]
               :when (= (dec (count id1)) (count common))]
           common)))

(defn solve [input]
  (let [ids (str/split-lines input)]
    (println "Part 1:" (checksum ids))
    (println "Part 2:" (find-similar-ids ids))))

(defn -main []
  (solve (slurp "input.txt")))

(-main)
