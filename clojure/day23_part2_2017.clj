
(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn is-prime [n]
  (loop [i 2]
    (if (<= (* i i) n)
      (if (zero? (rem n i))
        false
        (recur (inc i)))
      true)))

(defn -main []
  (let [input (slurp "input.txt")
        [b c] [(+ (* 57 100) 100000) (+ (+ (* 57 100) 100000) 17000)]
        h (loop [x b h 0]
            (if (<= x c)
              (recur (+ x 17) (if (not (is-prime x)) (inc h) h))
              h))]
    (println h)))

(-main)
