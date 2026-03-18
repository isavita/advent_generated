
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn knot-hash [input]
  (let [lengths (concat (map int input) [17 31 73 47 23])
        nums (vec (range 256))]
    (loop [nums nums, pos 0, skip 0, round 0]
      (if (= round 64)
        (let [dense-hash (for [i (range 16)]
                          (reduce bit-xor (subvec nums (* i 16) (* (inc i) 16))))]
          (apply str (map #(format "%08d" (Integer/parseInt (Integer/toBinaryString %))) dense-hash)))
        (let [[new-nums new-pos new-skip]
              (loop [nums nums, pos pos, skip skip, lengths lengths]
                (if (empty? lengths)
                  [nums pos skip]
                  (let [len (first lengths)
                        new-nums (if (zero? len)
                                   nums
                                   (let [start pos
                                         end (mod (+ pos len) 256)
                                         indices (for [i (range len)]
                                                   (mod (+ pos i) 256))
                                         reversed (reverse (map nums indices))
                                         new-nums (reduce (fn [n [idx val]]
                                                           (assoc n idx val))
                                                         nums
                                                         (map vector indices reversed))]
                                     new-nums))]
                    (recur new-nums
                           (mod (+ pos len skip) 256)
                           (inc skip)
                           (rest lengths)))))]
          (recur new-nums new-pos new-skip (inc round)))))))

(defn solve []
  (let [key (str/trim (slurp "input.txt"))
        grid (for [i (range 128)]
               (knot-hash (str key "-" i)))]
    (reduce + (map #(count (filter #{\1} %)) grid))))

(println (solve))
