(ns password-cracker
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn generate-hashes [door-id]
  (map #(md5 (str door-id %)) (iterate inc 0)))

(defn valid-hash? [hash]
  (.startsWith hash "00000"))

(defn extract-character [hash]
  (.charAt hash 5))

(defn find-password [door-id]
  (->> (generate-hashes door-id)
       (filter valid-hash?)
       (map extract-character)
       (take 8)
       (apply str)))

(defn -main []
  (let [door-id (-> "input.txt" slurp .trim)]
    (println (find-password door-id))))

(-main)
