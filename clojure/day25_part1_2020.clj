(ns encryption
  (:require [clojure.java.io :as io]))

(defn transform [subject-number loop-size]
  (mod (reduce (fn [v _] (mod (* v subject-number) 20201227)) 1 (range loop-size)) 20201227))

(defn find-loop-size [public-key]
  (loop [value 1 loop-size 0]
    (if (= value public-key)
      loop-size
      (recur (mod (* value 7) 20201227) (inc loop-size)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [card-public-key (Integer/parseInt (.readLine rdr))
          door-public-key (Integer/parseInt (.readLine rdr))]
      (let [card-loop-size (find-loop-size card-public-key)
            encryption-key (transform door-public-key card-loop-size)]
        (println encryption-key)))))

(-main)