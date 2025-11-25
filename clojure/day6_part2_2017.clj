
(ns redist
  (:require [clojure.string :as str]))

(defn -main [& _]
  (let [banks (vec (map #(Long/parseLong %) (re-seq #"\d+" (slurp "input.txt"))))]
    (loop [seen {}
           banks banks
           steps 0]
      (if-let [prev (get seen banks)]
        (do (println steps)
            (println (- steps prev)))
        (let [max-val (apply max banks)
              idx (loop [i 0]
                    (if (= (nth banks i) max-val) i (recur (inc i))))
              share max-val
              len (count banks)]
          (recur
            (assoc seen banks steps)
            (loop [i (mod (inc idx) len)
                   b (assoc banks idx 0)
                   r share]
              (if (zero? r)
                b
                (recur (mod (inc i) len) (update b i inc) (dec r))))
            (inc steps)))))))

(-main)
