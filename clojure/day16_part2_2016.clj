(ns dragon-checksum
  (:gen-class))

(defn dragon-curve [s]
  (str s "0" (apply str (map #(if (= % \0) \1 \0) (reverse s)))))

(defn generate-data [initial-state length]
  (loop [s initial-state]
    (if (>= (count s) length)
      (subs s 0 length)
      (recur (dragon-curve s)))))

(defn checksum [s]
  (loop [s s]
    (let [checksum (apply str (map #(if (= (nth % 0) (nth % 1)) \1 \0) (partition 2 s)))]
      (if (odd? (count checksum))
        checksum
        (recur checksum)))))

(defn -main []
  (let [initial-state (slurp "input.txt")
        disk-lengths [272 35651584]]
    (doseq [length disk-lengths]
      (let [data (generate-data initial-state length)
            checksum (checksum data)]
        (println "Checksum for disk of length" length ":" checksum)))))

(-main)