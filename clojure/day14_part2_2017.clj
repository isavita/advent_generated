
(ns solution
  (:require [clojure.string :as str]))

(defn knot-hash [s]
  (let [lengths (concat (map int s) [17 31 73 47 23])
        nums (int-array (range 256))]
    (loop [r 0 p 0 sk 0]
      (if (< r 64)
        (let [[np nsk] (reduce (fn [[cp csk] len]
                                 (dotimes [i (quot len 2)]
                                   (let [i1 (mod (+ cp i) 256)
                                         i2 (mod (+ cp len -1 (- i)) 256)
                                         v1 (aget nums i1)]
                                     (aset nums i1 (aget nums i2))
                                     (aset nums i2 v1)))
                                 [(mod (+ cp len csk) 256) (inc csk)])
                               [p sk] lengths)]
          (recur (inc r) np nsk))
        (apply str (for [i (range 16)]
                     (format "%02x" (reduce bit-xor (for [j (range 16)] (aget nums (+ (* i 16) j)))))))))))

(defn hex-to-bits [hex]
  (mapcat (fn [c]
            (let [v (Character/digit (char c) 16)]
              (for [i (range 3 -1 -1)] (if (bit-test v i) 1 0))))
          hex))

(let [key (str/trim (slurp "input.txt"))
      grid (vec (for [i (range 128)]
                  (vec (hex-to-bits (knot-hash (str key "-" i))))))
      total-used (reduce + (map #(reduce + %) grid))
      visited (boolean-array 16384)
      regions (atom 0)]
  (dotimes [y 128]
    (dotimes [x 128]
      (when (and (= 1 (get-in grid [y x])) (not (aget visited (+ (* y 128) x))))
        (swap! regions inc)
        (loop [st [[y x]]]
          (when (seq st)
            (let [[cy cx] (peek st)
                  nst (pop st)
                  idx (+ (* cy 128) cx)]
              (if (and (< -1 cy 128) (< -1 cx 128)
                       (= 1 (get-in grid [cy cx]))
                       (not (aget visited idx)))
                (do
                  (aset visited idx true)
                  (recur (conj nst [(inc cy) cx] [(dec cy) cx] [cy (inc cx)] [cy (dec cx)])))
                (recur nst))))))))
  (println "Total used squares:" total-used)
  (println "Number of regions:" @regions))
