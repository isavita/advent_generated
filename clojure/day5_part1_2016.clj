
(ns advent
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn md5-hex [s]
  (let [digest (doto (MessageDigest/getInstance "MD5")
                 (.update (.getBytes s "UTF-8")))
        bytes (.digest digest)]
    (apply str (map #(format "%02x" %) bytes))))

(defn door-password [door-id]
  (loop [idx 0 pwd []]
    (if (= (count pwd) 8)
      (apply str pwd)
      (let [hash (md5-hex (str door-id idx))]
        (if (and (= (subs hash 0 5) "00000"))
          (recur (inc idx) (conj pwd (nth hash 5)))
          (recur (inc idx) pwd))))))

(defn -main [& _]
  (let [door-id (str/trim (slurp "input.txt"))]
    (println (door-password door-id))))

(-main)
