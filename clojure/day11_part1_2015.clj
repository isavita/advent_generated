
(ns password
  (:require [clojure.java.io :as io]))

(defn read-input [file]
  (first (line-seq (io/reader file))))

(defn inc-char [c]
  (if (= c \z) \a (char (inc (int c)))))

(defn next-password [^String p]
  (let [a (.toCharArray p)]
    (loop [i (dec (alength a))]
      (aset a i (inc-char (aget a i)))
      (if (and (= (aget a i) \a) (pos? i))
        (recur (dec i))
        (String. a)))))

(defn valid? [^String p]
  (and
    (loop [i 2]
      (if (< i (count p))
        (let [a (int (.charAt p (dec (dec i))))
              b (int (.charAt p (dec i)))
              c (int (.charAt p i))]
          (if (= c (inc b) (inc (inc a)))
            true
            (recur (inc i))))
        false))
    (zero? (count (filter #(#{\i \o \l} %) p)))
    (>= (count (distinct (filter #(= (first %) (second %))
                                  (partition 2 1 p))))
        2)))

(defn next-valid [p]
  (loop [p p]
    (let [n (next-password p)]
      (if (valid? n)
        n
        (recur n)))))

(defn -main [& _]
  (let [start (read-input "input.txt")]
    (println (next-valid start))))

(-main)
