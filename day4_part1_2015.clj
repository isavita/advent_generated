(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.pprint :refer [pprint]])

(defn md5 [s]
  (let [md5 (java.security.MessageDigest/getInstance "MD5")
        bytes (.digest md5 (.getBytes s "UTF-8"))]
    (->> bytes
         (map #(format "%02x" %))
         (apply str))))

(defn find-advent-coin [input]
  (loop [n 1]
    (if (.startsWith (md5 (str input n)) "00000")
      n
      (recur (inc n)))))

(let [input (slurp "input.txt")]
  (println (find-advent-coin input)))
