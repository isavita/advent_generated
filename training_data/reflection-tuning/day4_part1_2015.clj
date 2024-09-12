(ns adventcoin.core
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn md5-hash [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn starts-with-five-zeros? [^String s]
  (.startsWith s "00000"))

(defn find-advent-coin [secret-key]
  (->> (iterate inc 1)
       (map (fn [n] [n (md5-hash (str secret-key n))]))
       (filter (fn [[_ hash]] (starts-with-five-zeros? hash)))
       (first)
       (first)))

(defn solve []
  (let [secret-key (-> "input.txt" slurp .trim)]
    (println (find-advent-coin secret-key))))

(solve)
