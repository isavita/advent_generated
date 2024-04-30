(ns custom-customs
  (:require [clojure.set :as set]
             [clojure.java.io :as io]))

(defn read-input []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (line-seq rdr)
          groups (clojure.string/split (clojure.string/join "\n" lines) #"\n\n")]
      (map #(clojure.string/split % #"\n") groups))))

(defn part-1 [groups]
  (reduce + (map #(count (apply set/union (map set %))) groups)))

(defn part-2 [groups]
  (reduce + (map #(count (apply set/intersection (map set %))) groups)))

(defn -main []
  (let [groups (read-input)]
    (println "Part 1:" (part-1 groups))
    (println "Part 2:" (part-2 groups))))

(-main)