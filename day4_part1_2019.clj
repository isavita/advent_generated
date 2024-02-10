
(defn valid-password? [password]
  (let [digits (map #(Character/getNumericValue %) (str password))
        adjacent-same? (some #(apply = %) (partition 2 1 digits))
        increasing? (apply <= digits)]
    (and adjacent-same? increasing?)))

(defn count-valid-passwords [start end]
  (count (filter valid-password? (range start (inc end)))))

(def input (slurp "input.txt"))
(let [[start end] (map read-string (clojure.string/split input #"-"))]
  (println (count-valid-passwords start end)))
