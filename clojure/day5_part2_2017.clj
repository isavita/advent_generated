(ns maze
  (:require [clojure.java.io :as io]
             [clojure.string :as str]))

(defn navigate-maze []
  (let [lines (str/split (slurp "input.txt") #"\n")
        offsets (vec (map #(Integer/parseInt %) lines))
        offsets (atom offsets)
        index (atom 0)
        steps (atom 0)]
    (while (and (>= @index 0) (< @index (count @offsets)))
      (let [jump (nth @offsets @index)]
        (if (>= jump 3)
          (swap! offsets (fn [v] (assoc v @index (dec jump))))
          (swap! offsets (fn [v] (assoc v @index (inc jump)))))
        (reset! index (+ @index jump))
        (swap! steps inc)))
    (println @steps)))

(navigate-maze)