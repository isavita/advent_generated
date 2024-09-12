(defn count-increases [measurements]
  (->> measurements
       (partition 2 1)
       (filter (fn [[a b]] (> b a)))
       count))

(defn solve-puzzle []
  (-> "input.txt"
      slurp
      clojure.string/split-lines
      (->> (map #(Integer/parseInt %)))
      count-increases
      println))

(solve-puzzle)
