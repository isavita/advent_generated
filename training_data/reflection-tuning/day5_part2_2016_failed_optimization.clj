(ns day5
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn interesting-hash? [hash]
  (str/starts-with? hash "00000"))

(defn generate-hashes [door-id]
  (let [base (str door-id)]
    (keep-indexed
     (fn [idx _]
       (let [hash (md5 (str base idx))]
         (when (interesting-hash? hash)
           [idx hash])))
     (range))))

(defn solve-part1 [door-id]
  (->> (generate-hashes door-id)
       (take 8)
       (map #(nth (second %) 5))
       (apply str)))

(defn solve-part2 [door-id]
  (loop [hashes (generate-hashes door-id)
         password (transient (vec (repeat 8 nil)))]
    (if (every? some? password)
      (apply str (persistent! password))
      (let [[_ hash] (first hashes)
            pos (Character/digit (nth hash 5) 16)
            char (nth hash 6)]
        (if (and (< pos 8) (nil? (nth password pos)))
          (recur (rest hashes) (assoc! password pos char))
          (recur (rest hashes) password))))))

(defn -main [& args]
  (let [door-id "abc"]  ; Replace with actual door ID
    (println "Part 1:" (solve-part1 door-id))
    (println "Part 2:" (solve-part2 door-id))))
