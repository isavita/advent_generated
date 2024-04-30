(ns firewall
  (:require [clojure.java.io :as io]))

(defn parse-input [file]
  (with-open [r (io/reader file)]
    (reduce (fn [m line]
              (let [[depth rng] (map #(Integer/parseInt %) (.split line ": "))]
                (assoc m depth {:range rng :position 0 :direction 1})))
            {}
            (line-seq r))))

(defn pass-through [firewall delay]
  (every? (fn [[depth {:keys [range]}]]
            (not= (mod (+ depth delay) (* 2 (dec range))) 0))
          firewall))

(defn -main []
  (let [firewall (parse-input "input.txt")
        delay (first (drop-while #(not (pass-through firewall %)) (range)))]
    (prn delay)))

(-main)