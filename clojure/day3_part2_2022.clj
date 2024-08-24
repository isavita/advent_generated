(ns solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn get-priority [c]
  (if (Character/isLowerCase c)
    (+ 1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(defn read-input []
  (with-open [r (io/reader "input.txt")]
    (doall (line-seq r))))

(defn part1 []
  (let [lines (read-input)]
    (reduce + (map (fn [line]
                      (let [len (count line)
                            left (subs line 0 (/ len 2))
                            right (subs line (/ len 2))]
                        (get-priority (first (set/intersection (set left) (set right))))))
                    lines))))

(defn part2 []
  (let [lines (read-input)
        groups (partition 3 lines)]
    (reduce + (map (fn [group]
                      (let [common (apply set/intersection (map set group))]
                        (get-priority (first common))))
                    groups))))

(println (part1))
(println (part2))