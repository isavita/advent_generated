
(ns invalid-ids
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.math BigInteger]))

(defn parse-range [s]
  (let [[l r] (str/split s #"-")]
    [(BigInteger. l) (BigInteger. r)]))

(defn sum-invalid-ids [ranges]
  (let [found (atom #{})]
    (doseq [[start end] ranges]
      (doseq [k (range 1 11)]
        (let [multiplier (.add (.pow (BigInteger/TEN) k) BigInteger/ONE)
              min-seed   (.pow (BigInteger/TEN) (dec k))
              max-seed   (.subtract (.pow (BigInteger/TEN) k) BigInteger/ONE)
              s-min      (.divide (.add (.subtract start BigInteger/ONE) multiplier) multiplier)
              s-max      (.divide end multiplier)
              start-seed (.max s-min min-seed)
              end-seed   (.min s-max max-seed)]
          (loop [seed start-seed]
            (when (<= (.compareTo seed end-seed) 0)
              (swap! found conj (.multiply seed multiplier))
              (recur (.add seed BigInteger/ONE)))))))
    (reduce #(.add %1 %2) BigInteger/ZERO @found)))

(defn -main [& _]
  (let [txt   (str/trim (str/replace (slurp "input.txt") #"\r|\n" ""))
        pairs (remove str/blank? (str/split txt #","))]
    (println (sum-invalid-ids (map parse-range pairs)))))

(-main)
