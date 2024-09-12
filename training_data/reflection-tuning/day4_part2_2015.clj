(ns advent-of-code.day4
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn md5-hash [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn starts-with-zeroes? [s n]
  (every? #(= % \0) (take n s)))

(defn find-advent-coin [secret-key zero-count]
  (->> (iterate inc 1)
       (map #(vector % (md5-hash (str secret-key %))))
       (filter #(starts-with-zeroes? (second %) zero-count))
       (first)
       (first)))

(defn solve [input]
  (let [secret-key (clojure.string/trim-newline (slurp input))]
    (println "Part One:" (find-advent-coin secret-key 5))
    (println "Part Two:" (find-advent-coin secret-key 6))))

(solve "input.txt")
