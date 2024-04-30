(ns dragon-checksum
  (:gen-class))

(defn dragon-curve [s]
  (let [b (clojure.string/reverse s)
        b (clojure.string/replace b "0" "x")
        b (clojure.string/replace b "1" "0")
        b (clojure.string/replace b "x" "1")]
    (str s "0" b)))

(defn generate-data [initial-state length]
  (loop [s initial-state]
    (if (>= (count s) length)
      (subs s 0 length)
      (recur (dragon-curve s)))))

(defn checksum [s]
  (loop [s s]
    (let [checksum (apply str (map #(if (= (nth % 0) (nth % 1)) "1" "0") (partition 2 s)))]
      (if (odd? (count checksum))
        checksum
        (recur checksum)))))

(defn -main []
  (let [input (slurp "input.txt")
        data (generate-data input 272)
        checksum (checksum data)]
    (println checksum)))

(-main)