(ns day5
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn valid-hash? [hash]
  (= (subs hash 0 5) "00000"))

(defn generate-hashes [door-id]
  (map (fn [i] [(md5 (str door-id i)) i])
       (iterate inc 0)))

(defn find-password [door-id]
  (->> (generate-hashes door-id)
       (filter (comp valid-hash? first))
       (map (comp #(nth % 5) first))
       (take 8)
       (apply str)))

(defn find-password-part2 [door-id]
  (loop [hashes (generate-hashes door-id)
         password (vec (repeat 8 \_))]
    (if (every? #(not= % \_) password)
      (apply str password)
      (let [[hash _] (first (filter (comp valid-hash? first) hashes))
            pos (Character/digit (nth hash 5) 16)
            char (nth hash 6)]
        (if (and (< pos 8) (= (nth password pos) \_))
          (recur (rest hashes) (assoc password pos char))
          (recur (rest hashes) password))))))

(defn solve []
  (let [door-id (clojure.string/trim (slurp "input.txt"))]
    (println "Part 1:" (find-password door-id))
    (println "Part 2:" (find-password-part2 door-id))))

(solve)
