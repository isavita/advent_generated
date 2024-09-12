(def shape-scores {"X" 1, "Y" 2, "Z" 3})
(def outcome-scores {:win 6, :draw 3, :lose 0})

(defn determine-outcome [opponent me]
  (case [opponent me]
    ["A" "X"] :draw
    ["A" "Y"] :win
    ["A" "Z"] :lose
    ["B" "X"] :lose
    ["B" "Y"] :draw
    ["B" "Z"] :win
    ["C" "X"] :win
    ["C" "Y"] :lose
    ["C" "Z"] :draw))

(defn calculate-round-score [[opponent me]]
  (+ (shape-scores me)
     (outcome-scores (determine-outcome opponent me))))

(defn solve-rock-paper-scissors [input]
  (->> input
       (line-seq)
       (map #(clojure.string/split % #" "))
       (map calculate-round-score)
       (reduce +)))

(println (solve-rock-paper-scissors (clojure.java.io/reader "input.txt")))
