
(defn parse-card [line]
  (let [[_ winning-str have-str] (re-matches #"Card\s+\d+:\s+([\d\s]+)\|\s+([\d\s]+)" line)
        winning-nums (set (map parse-long (clojure.string/split winning-str #"\s+")))
        have-nums (map parse-long (clojure.string/split have-str #"\s+"))]
    {:winning winning-nums :have have-nums}))

(defn calculate-card-score [{:keys [winning have]}]
  (let [matches (count (filter winning have))]
    (if (pos? matches)
      (bit-shift-left 1 (dec matches))
      0)))

(defn solve [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> (line-seq rdr)
         (map parse-card)
         (map calculate-card-score)
         (reduce +))))

(println (solve "input.txt"))
