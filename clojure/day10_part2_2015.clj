
(require '[clojure.string :as str])

(defn look-and-say [s]
  (apply str (mapcat #(vector (count %) (first %)) (partition-by identity s))))

(defn solve [s n]
  (if (zero? n)
    (count s)
    (recur (look-and-say s) (dec n))))

(with-open [rdr (clojure.java.io/reader "input.txt")]
  (let [input (str/trim (first (line-seq rdr)))]
    (println (solve input 40))
    (println (solve input 50)))
)
