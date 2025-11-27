
(ns quantum
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [file]
  (vec (map #(Long/parseLong %) (str/split-lines (slurp file)))))

(defn qe [g] (reduce *' g))

(defn total [v] (reduce + v))

(defn solve [weights groups]
  (let [target (/ (total weights) groups)
        n (count weights)
        best (atom nil)]
    (letfn [(dfs [idx used cnt sum prod]
              (cond
                (= sum target)
                (when (or (nil? @best)
                          (< cnt (:cnt @best))
                          (and (= cnt (:cnt @best))
                               (< prod (:qe @best))))
                  (reset! best {:cnt cnt :qe prod}))
                (or (>= sum target) (>= idx n)) nil
                :else
                (do
                  (dfs (inc idx) (conj used (nth weights idx)) (inc cnt)
                       (+ sum (nth weights idx)) (*' prod (nth weights idx)))
                  (dfs (inc idx) used cnt sum prod))))]
      (dfs 0 [] 0 0 1)
      (:qe @best))))

(let [w (read-input "input.txt")]
  (println (solve w 3))
  (println (solve w 4)))
