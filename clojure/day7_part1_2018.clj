
(ns day7
  (:require [clojure.string :as str]))

(defn parse [line]
  (let [[_ a b] (re-matches #"Step (\w) must be finished before step (\w) can begin\." line)]
    [(first a) (first b)]))

(defn topo [deps]
  (loop [deps deps, avail (sort (set (flatten (seq deps)))), done []]
    (if (empty? avail)
      done
      (let [nxt (first (filter #(not-any? (fn [[_ b]] (= b %)) deps) avail))]
        (recur (remove (fn [[a _]] (= a nxt)) deps)
               (disj (set avail) nxt)
               (conj done nxt))))))

(->> (slurp "input.txt")
     str/split-lines
     (map parse)
     vec
     topo
     (apply str)
     println)
