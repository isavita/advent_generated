
(ns advent.main
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn compare* [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (compare* [a] b)
    (number? b) (compare* a [b])
    :else (loop [aa a bb b]
            (cond
              (and (empty? aa) (empty? bb)) 0
              (empty? aa) -1
              (empty? bb) 1
              :else (let [c (compare* (first aa) (first bb))]
                      (if (not= c 0)
                        c
                        (recur (rest aa) (rest bb))))))))

(defn solve [input]
  (->> (str/split input #"\n\n")
       (map-indexed
        (fn [i pair]
          (let [[left right] (map edn/read-string (str/split pair #"\n"))]
            (if (= -1 (compare* left right))
              (inc i)
              0))))
       (reduce +)))

(defn main []
  (let [input (slurp "input.txt")]
    (println (solve input))))

(main)
