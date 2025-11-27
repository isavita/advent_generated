
(ns marble-mania
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input []
  (let [line (str/trim (slurp "input.txt"))
        nums (map #(Long/parseLong %) (re-seq #"\d+" line))]
    [(first nums) (second nums)]))

(defn play-game [players last]
  (let [scores (long-array players)]
    (loop [circle (vec '(0))
           current 0
           marble 1]
      (if (> marble last)
        (apply max (seq scores))
        (let [player (mod (dec marble) players)]
          (if (zero? (mod marble 23))
            (let [remove-idx (mod (- current 7) (count circle))
                  removed (nth circle remove-idx)]
              (aset-long scores player (+ marble removed (aget scores player)))
              (recur (vec (concat (subvec circle 0 remove-idx)
                                  (subvec circle (inc remove-idx))))
                     remove-idx
                     (inc marble)))
            (let [insert-idx (mod (inc current) (count circle))]
              (recur (vec (concat (subvec circle 0 (inc insert-idx))
                                  (list marble)
                                  (subvec circle (inc insert-idx))))
                     (inc insert-idx)
                     (inc marble)))))))))

(defn -main [& args]
  (let [[players last] (parse-input)]
    (println (play-game players last))))

(-main)
