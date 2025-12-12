
(ns joltage
  (:require [clojure.java.io :as io]))

(defn calc-max [^String s]
  (let [n (count s)]
    (loop [d1 9]
      (if (neg? d1) 0
        (let [idx (clojure.string/index-of s (char (+ d1 48)))]
          (if (and idx (< (inc idx) n))
            (let [tail (subs s (inc idx))
                  d2 (transduce (comp (filter #(Character/isDigit %))
                                      (map #(unchecked-subtract (int %) 48)))
                                (completing (fn [a b] (if (>= b 9) (reduced 9) (max a b))))
                                -1 tail)]
              (if (neg? d2) (recur (dec d1)) (+ (* d1 10) d2)))
            (recur (dec d1))))))))

(defn -main [& _]
  (with-open [rdr (io/reader "input.txt")]
    (println
      (transduce (comp (map clojure.string/trim)
                       (remove empty?)
                       (map calc-max))
                 + (line-seq rdr)))))

(-main)
