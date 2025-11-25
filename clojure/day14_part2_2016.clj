
(ns advent
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(def ^MessageDigest md5 (MessageDigest/getInstance "MD5"))

(def cache (atom {}))

(defn md5-hex [^String s]
  (.reset md5)
  (.update md5 (.getBytes s))
  (format "%032x" (BigInteger. 1 (.digest md5))))

(defn stretch [salt]
  (if-let [c (get @cache salt)]
    c
    (let [h (reduce (fn [h _] (md5-hex h))
                    (md5-hex salt)
                    (range 2016))]
      (swap! cache assoc salt h)
      h)))

(defn triplet [^String h]
  (loop [i 0]
    (when (< (inc i) (dec (count h)))
      (let [c (.charAt h i)]
        (if (and (= c (.charAt h (inc i)))
                 (= c (.charAt h (+ i 2))))
          c
          (recur (inc i)))))))

(defn has-quint [^String h c]
  (let [needle (str c c c c c)]
    (str/includes? h needle)))

(defn -main [& _]
  (let [salt (str/trim (slurp "input.txt"))
        [idx _]
        (loop [idx 0 keys 0]
          (if (= keys 64)
            [idx keys]
            (let [h (stretch (str salt idx))]
              (if-let [t (triplet h)]
                (if (some #(has-quint (stretch (str salt %)) t)
                          (range (inc idx) (+ idx 1001)))
                  (recur (inc idx) (inc keys))
                  (recur (inc idx) keys))
                (recur (inc idx) keys)))))]
    (println (dec idx))))

(-main)
