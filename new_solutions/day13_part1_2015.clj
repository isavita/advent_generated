(ns optimal-seating
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ person gain-loss units neighbor] (re-matches #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)." line)]
    [(keyword person) (keyword neighbor) (* (if (= gain-loss "gain") 1 -1) (Integer/parseInt units))]))

(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce (fn [acc line]
              (let [[person neighbor units] (parse-line line)]
                (update acc person #(assoc (or % {}) neighbor units))))
            {}
            (line-seq rdr))))

(defn calculate-happiness [seating happiness-map]
  (reduce (fn [total [person neighbor]]
            (+ total (get-in happiness-map [person neighbor] 0) (get-in happiness-map [neighbor person] 0)))
          0
          (map vector seating (concat (rest seating) [(first seating)]))))

(defn permutations [coll]
  (if (empty? coll)
    '(())
    (for [x coll
          perm (permutations (remove #{x} coll))]
      (cons x perm))))

(defn find-optimal-seating [happiness-map]
  (let [people (keys happiness-map)]
    (apply max (map #(calculate-happiness % happiness-map) (permutations people)))))

(defn -main []
  (let [happiness-map (read-input "input.txt")]
    (println (find-optimal-seating happiness-map))))

(-main)