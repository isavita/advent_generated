
(ns light-beam
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn main [& _args]
  (let [grid   (filter seq (str/split-lines (slurp "input.txt")))
        height (count grid)
        width  (count (first grid))
        start-y (first (keep-indexed #(when (str/index-of %2 \S) %1) grid))
        start-x (str/index-of (nth grid start-y) \S)]
    (if (nil? start-x)
      (throw (ex-info "Start point 'S' not found" {}))
      (let [total-splits (atom 0)]
        (loop [y start-y
               beams #{start-x}]
          (if (or (>= y height) (empty? beams))
            (println "Total times the beam is split:" @total-splits)
            (let [next-beams (atom #{})]
              (doseq [x beams
                      :when (< -1 x width)
                      :let [cell (nth (nth grid y) x)]]
                (if (= cell \^)
                  (do (swap! total-splits inc)
                      (when (< 0 x) (swap! next-beams conj (dec x)))
                      (when (< (inc x) width) (swap! next-beams conj (inc x))))
                  (swap! next-beams conj x)))
              (recur (inc y) @next-beams))))))))
(main)
