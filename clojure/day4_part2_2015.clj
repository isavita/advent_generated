(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(defn md5 [input]
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (.getBytes input))
    (.digest md)))

(defn to-hex [byte-array]
  (apply str (map (fn [b] (format "%02x" b)) byte-array)))

(defn solve []
  (let [data (-> (io/reader "input.txt")
                 (slurp)
                 (str/trim))]
    (loop [number 0]
      (let [hash (md5 (str data number))
            hash-string (to-hex hash)]
        (if (str/starts-with? hash-string "000000")
          (println number)
          (recur (inc number)))))))

(solve)
