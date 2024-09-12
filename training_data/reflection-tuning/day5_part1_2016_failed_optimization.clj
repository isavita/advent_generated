(ns day5.core
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn starts-with-five-zeros? [^String s]
  (= "00000" (subs s 0 5)))

(defn generate-password [door-id]
  (->> (range)
       (map (fn [i] [i (md5 (str door-id i))]))
       (filter (fn [[_ hash]] (starts-with-five-zeros? hash)))
       (map (fn [[_ hash]] (nth hash 5)))
       (take 8)
       (apply str)))

(defn solve [door-id]
  (generate-password door-id))

;; Example usage:
;; (println (solve "abc"))  ; Should print "18f47a30"
;; (println (solve "your-actual-door-id"))
